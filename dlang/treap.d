import std.random;

//flags: +1 - has value
//flags: +2 - has size
class Treap(Key, Value, int flags = 3) {
  private:
  static struct Node {
    Node *left;
    Node *right;
    Key x;
    static if (flags & 1) Value value;
    int y;
    static if (flags & 2) int sz;
  }
  static Node free_nodes;
  static this () { free_nodes.left = free_nodes.right = &free_nodes; }
  static Node *newNode () {
    Node *p = free_nodes.right;
    if (p == &free_nodes) {
      p = new Node ();
    } else {
      p.left.right = p.right; p.right.left = p.left;
    }
    static if (flags & 2) p.sz = 1;
    return p;
  }

  Node *root;
  static if (flags & 2) {
    static int _size (const Node *t) {
      return t ? t.sz : 0;
    }
    static void _relax (Node *t) {
      t.sz = 1;
      if (t.left) t.sz += t.left.sz;
      if (t.right) t.sz += t.right.sz;
    }
    static Node *_kthNode (Node *t, size_t k) {
      if (k >= _size (t)) return null;
      while (t) {
        immutable ls = _size (t.left);
        if (k > ls) {
          k -= ls + 1;
          t = t.right;
        } else if (k < ls) {
          t = t.left;
        } else {
          return t;
        }
      }
      assert (false);
    }
  }
  static Node *_find (Node *t, Key x) {
    while (t && t.x != x) {
      t = (x < t.x) ? t.left : t.right;
    }
    return t;
  }

  static void _split (Node *t, Key x, ref Node* l, ref Node* r) {
    if (!t) {
      l = r = null;
      return;
    }
    if (x < t.x) {
      r = t;
      _split (t.left, x, l, t.left);
    } else {
      l = t;
      _split (t.right, x, t.right, r);
    }
    static if (flags & 2) _relax (t);
  }

  static Node *_insert (Node *t, Node *p) {
    if (!t) {
      p.left = p.right = null;
      return p;
    }
    if (t.y >= p.y) {
      if (p.x < t.x) {
        t.left = _insert (t.left, p);
      } else {
        t.right = _insert (t.right, p);
      }
      static if (flags & 2) ++t.sz;
      return t;
    }
    _split (t, p.x, p.left, p.right);
    static if (flags & 2) _relax (p);
    return p;
  }

  static Node *_merge (Node *l, Node *r) {
    if (!l) {
      return r;
    } else if (!r) {
      return l;
    } else if (l.y > r.y) {
      l.right = _merge (l.right, r);
      static if (flags & 2) _relax (l);
      return l;
    } else {
      r.left = _merge (l, r.left);
      static if (flags & 2) _relax (r);
      return r;
    }
  }

  static Node *_remove (ref Node **w, Key x, ) {
    static if (flags & 2) {
      Node*[128] path = void;
      int m = -1;
    }
    Node **p = w;
    Node *t = *p;
    while (t) {
      static if (flags & 2) {
        if (t.x == x) {
          foreach (i; 0 .. m + 1) {
            --path[i].sz;
          }
          w = p;
          return t;
        }
        path[++m] = t;
      } else {
        if (t.x == x) {
          w = p;
          return t;
        }
      }
      p = (x < t.x) ? &(t.left) : &(t.right);
      t = *p;
    }
    return null;
  }

  public:

  static if (flags & 1) {
    final void insert (Key key, Value value) {
      Node *p = newNode ();
      p.x = key;
      p.y = uniform (int.min, int.max);
      p.value = value;
      root = _insert (root, p);
    }
  } else {
    final void insert (Key key) {
      Node *p = newNode ();
      p.x = key;
      p.y = uniform (int.min, int.max);
      root = _insert (root, p);
    }
  }

  final bool remove (Key x) {
    Node **w = &root;
    auto t = _remove (w, x);
    if (!t) return false;
    *w =  _merge (t.left, t.right);
    Node *u = free_nodes.left, v = &free_nodes;
    u.right = t; t.left = u;
    t.right = v; v.left = t;
    return true;
  }

  final bool contains (Key x) { return _find (root, x) !is null; }
}

class ImplicitKeyTreap(Value) {
  Node root;
  static class Node {
    Node left, right;
    immutable Value value;
    immutable int y;
    int sz;
    this (Value _value) {
      y = uniform (int.min, int.max);
      value = _value;
      sz = 1;
    }
  }

  private static int _getSize (const Node t) {
    return t ? t.sz : 0;
  }

  private static void relax (Node t) {
    if (t) {
      t.sz = 1 + _getSize (t.left) + _getSize (t.right);
    }
  }
  private static void _split (Node t, ref Node l, ref Node r, int key, int s) {
    if (!t) {
      l = r = null;
      return;
    }
    immutable ls = _getSize (t.left);
    if (key <= s + ls) {
      r = t;
      _split (t.left, l, t.left, key, s);
    } else {
      l = t;
      _split (t.right, t.right, r, key, s + 1 + ls);
    }
    relax (t);
  }

  private static Node _merge (Node l, Node r) {
    Node t;
    if (!l) {
      t = r;
    } else if (!r) {
      t = l;
    } else if (l.y > r.y) {
      t = l;
      l.right = _merge (l.right, r);
    } else {
      t = r;
      r.left = _merge (l, r.left);
    }
    relax (t);
    return t;
  }

  private static inout(Node) _get (inout(Node) t, int pos) in {
    assert (pos >= 0);
    assert (pos < _getSize (t));
    assert (t);
  } body {
    if (t.left) {
      if (pos < t.left.sz) {
        return _get (t.left, pos);
      }
      pos -= t.left.sz;
    }
    if (!pos) {
      return t;
    }
    return _get (t.right, pos - 1);
  }

  private static Node _remove (Node t, int pos, ref Node parent, ref bool right_path) in {
    assert (pos >= 0);
    assert (pos < _getSize (t));
    assert (t);
  } body {
    if (t.left) {
      if (pos < t.left.sz) {
        parent = t;
        right_path = false;
        --t.sz;
        return _remove (t.left, pos, parent, right_path);
      }
      pos -= t.left.sz;
    }
    if (!pos) {
      return t;
    }
    parent = t;
    right_path = true;
    --t.sz;
    return _remove (t.right, pos - 1, parent, right_path);
  }

  final void insert (int pos, Value value) {
    Node tl, tr;
    _split (root, tl, tr, pos, 0);
    root = _merge (_merge (tl, new Node (value)), tr);
  }

  final void remove (int pos) {
    Node parent;
    bool right_path;
    auto t = _remove (root, pos, parent, right_path);
    Node p = _merge (t.left, t.right);
    if (parent) {
      right_path ? parent.right : parent.left = p;
    } else {
      root = p;
    }
  }

  final Value get (int pos) const in {
    assert (pos < getSize ());
  } body {
    return _get (root, pos).value;
  }

  final int getSize () const {
    return _getSize (root);
  }
}

unittest {
  auto t = new Treap!(int, long, 3);
  t.insert (1, 2);
  auto t2 = new Treap!(int, long, 2);
  t2.insert (1);
  assert (t2.contains (1));
  assert (t2.remove (1));
  assert (!t2.contains (1));

  auto t0 = new Treap!(int, int, 0);
  t0.insert (1);
  assert (t0.contains (1));
  assert (t0.remove (1));
  assert (!t0.contains (1));

}
