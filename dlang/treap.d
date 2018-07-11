import std.algorithm, std.conv, std.range;
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
    static size_t _countLess (Node *t, Key x) {
      size_t s;
      while (t) {
        if (t.x < x) {
          s += 1 + _size (t.left);
          t = t.right;
        } else {
          t = t.left;
        }
      }
      return s;
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
  static if (flags & 2) {
    final size_t countLess (Key x) { return _countLess (root, x); }
    final Key kthKey (size_t k) {
      Node *p = _kthNode (root, k);
      assert (p);
      return p.x;
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

class ImplicitKeyTreap(Value, string push_op="", string update_op="") {
  Node *root;
  static struct Node {
    Node *left;
    Node *right;
    Value value;
    int y;
    int sz;
  }
  static Node *newNode (Value _value) {
    Node *p = new Node;
    p.value = _value;
    p.y = uniform (int.min, int.max);
    p.sz = 1;
    return p;
  }

  static if (push_op != "") {
    private static void _push (Node *t) {
      mixin (push_op);
    }
  }

  private static int _size (const Node *t) {
    return t ? t.sz : 0;
  }

  private static void _relax (Node *t) {
    t.sz = 1 + _size (t.left) + _size (t.right);
  }

  private static void _split (Node *t, ref Node *l, ref Node *r, int key, int s) {
    if (!t) {
      l = r = null;
      return;
    }
    static if (push_op != "") _push (t);
    immutable ls = _size (t.left);
    if (key <= s + ls) {
      r = t;
      _split (t.left, l, t.left, key, s);
    } else {
      l = t;
      _split (t.right, t.right, r, key, s + 1 + ls);
    }
    _relax (t);
  }

  private static Node *_merge (Node *l, Node *r) {
    if (!l) {
      static if (push_op != "") if (r) _push (r);
      return r;
    } else if (!r) {
      static if (push_op != "") _push (l);
      return l;
    } else {
      static if (push_op != "") {
        _push (l);
        _push (r);
      }
      if (l.y > r.y) {
        l.right = _merge (l.right, r);
        _relax (l);
        return l;
      } else {
        r.left = _merge (l, r.left);
        _relax (r);
        return r;
      }
    }
  }

  private static inout(Node*) _get (inout(Node*) t, int pos) in {
    assert (pos >= 0);
    assert (pos < _size (t));
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

  private static Node *_remove (Node *t, int pos, ref Node *parent, ref bool right_path) in {
    assert (pos >= 0);
    assert (pos < _size (t));
    assert (t);
  } body {
    static if (push_op != "") _push (t);
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
    Node *tl, tr;
    _split (root, tl, tr, pos, 0);
    root = _merge (_merge (tl, newNode (value)), tr);
  }

  final void remove (int pos) {
    Node *parent;
    bool right_path;
    auto t = _remove (root, pos, parent, right_path);
    Node *p = _merge (t.left, t.right);
    if (parent) {
      right_path ? parent.right : parent.left = p;
    } else {
      root = p;
    }
  }

  final Value get (int pos) const in {
    assert (pos < size ());
  } body {
    return _get (root, pos).value;
  }

  final int size () const {
    return _size (root);
  }

  final Value[] values () {
    Value[] a = [];
    a.reserve (_size (root));
    void rec (Node *t) {
      static if (push_op != "") _push (t);
      if (t.left) rec (t.left);
      a ~= t.value;
      if (t.right) rec (t.right);
    }
    if (root) rec (root);
    return a;
  }

  static if (update_op != "") {
    final void update (size_t l, size_t r) {
      Node *t1, t2, t, t4;
      _split (root, t1, t2, l.to!int, 0);
      _split (t2, t, t4, (r - l).to!int, 0);
      mixin (update_op);
      root = _merge (t1, _merge (t, t4));
    }
  }
}

alias ReversingTreap = ImplicitKeyTreap!(int, q{
  if (t.value < 0) {
    swap (t.left, t.right);
    if (t.left) t.left.value *= -1;
    if (t.right) t.right.value *= -1;
    t.value *= -1;
  }
}, "t.value *= -1;");

unittest {
  import std.range, std.stdio;
  writeln ("Testing ", __FILE__, " ...");
  auto t = new Treap!(int, long, 3);
  t.insert (1, 2);
  auto t2 = new Treap!(int, long, 2);
  t2.insert (1);
  assert (t2.contains (1));
  assert (t2.kthKey (0) == 1);
  assert (t2.countLess (2) == 1);
  assert (t2.countLess (1) == 0);
  assert (t2.remove (1));
  assert (!t2.contains (1));

  auto t0 = new Treap!(int, int, 0);
  t0.insert (1);
  assert (t0.contains (1));
  assert (t0.remove (1));
  assert (!t0.contains (1));

  auto tr = new ReversingTreap;
  foreach (i; iota (1, 11)) tr.insert (i-1, i);
  tr.update (0, 10);
  assert (equal (tr.values (), retro (iota (1, 11))));
  tr.remove (0);
  assert (equal (tr.values (), retro (iota (1, 10))));
}
