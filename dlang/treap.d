import std.algorithm, std.conv, std.range;
import std.random;
import std.traits;

//flags: +1 - has value
//flags: +2 - has size
class Treap(Key, Value, int flags = 3) {
  static assert ((flags & 1) || is (Value == void));
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

  static Node *allocNode () {
    Node *p = free_nodes.right;
    if (p == &free_nodes) {
      p = new Node ();
    } else {
      p.left.right = p.right; p.right.left = p.left;
      p.left = p.right = null;
    }
    static if (flags & 2) p.sz = 1;
    return p;
  }
  static if (flags & 1) {
    static Node* newNode (Key key, Value value) {
      Node *p = allocNode ();
      p.x = key;
      p.y = uniform!int;
      p.value = value;
      return p;
    }
  } else {
    static Node* newNode (Key key) {
      Node *p = allocNode ();
      p.x = key;
      p.y = uniform!int;
      return p;
    }
  }

  static void _free (Node *t) {
    Node *u = free_nodes.left, v = &free_nodes;
    u.right = t; t.left = u;
    t.right = v; v.left = t;
  }

  static void _clear (Node *t) {
    if (t.left) _clear (t.left);
    if (t.right) _clear (t.right);
    _free (t);
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
      root = _insert (root, newNode (key, value));
    }
  } else {
    final void insert (Key key) {
      root = _insert (root, newNode (key));
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

  final void clear () {
    _clear (root);
    root = null;
  }

  final bool contains (Key x) { return _find (root, x) !is null; }
}

struct ImplicitKeyTreapNode(Value, Extra=void) {
  enum has_extra = !(is (Extra == void));
  ImplicitKeyTreapNode!(Value, Extra)* left, right;
  Value value;
  static if (has_extra) Extra extra;
  int y;
  int sz;
  this (Value _value) {
    value = _value;
    y = uniform!int;
    sz = 1;
  }
}

class ImplicitKeyTreap(Value, Extra=void, alias relax_op="", alias push_op="", alias update_op="") {
  enum has_push = isCallable!push_op;
  enum has_relax = isCallable!relax_op;
  enum has_update = isCallable!update_op;
  static assert (has_push == has_update);
  alias Node = ImplicitKeyTreapNode!(Value, Extra);
  Node *root;

  private static int _size (const Node *t) {
    return t ? t.sz : 0;
  }

  private static void _relax_inc (Node *t) {
    ++t.sz;
    static if (has_relax) relax_op (t);
  }

  private static void _relax_dec (Node *t) {
    --t.sz;
    static if (has_relax) relax_op (t);
  }

  private static void _relax (Node *t) {
    t.sz = 1;
    if (t.left) t.sz += t.left.sz;
    if (t.right) t.sz += t.right.sz;
    static if (has_relax) relax_op (t);
  }

  private static void _split (Node *t, ref Node *l, ref Node *r, int key) {
    if (!t) {
      l = r = null;
      return;
    }
    static if (has_push) push_op (t);
    int ls;
    if (t.left) ls = t.left.sz;
    if (key <= ls) {
      _split (t.left, l, t.left, key);
      r = t;
    } else {
      _split (t.right, t.right, r, key - ls - 1);
      l = t;
    }
    _relax (t);
  }

  private static void _build (ref Node *t, Value[] v) {
    if (v.length == 1) {
      t = new Node (v[0]);
      static if (has_relax) relax_op (t);
    } else {
      immutable m = v.length >> 1;
      Node* tl, tr;
      _build (tl, v[0 .. m]);
      _build (tr, v[m .. $]);
      _merge (t, tl, tr);
    }
  }

  private static void _insert (ref Node *t, Node *p, int pos) {
	if (!t) {
      static if (has_relax) relax_op (p);
	  t = p;
      return;
    } else if (p.y >= t.y) {
      _split (t, p.left, p.right, pos);
      t = p;
      _relax (t);
    } else {
      int ls;
      if (t.left) ls = t.left.sz;
      if (pos <= ls) {
        _insert (t.left, p, pos);
        _relax_inc (t);
      } else {
        _insert (t.right, p, pos - ls - 1);
        _relax_inc (t);
      }
    }
  }

  private static void _merge (ref Node *t, Node *l, Node *r) {
    if (!l) {
      static if (has_push) if (r) push_op (r);
      t = r;
    } else if (!r) {
      static if (has_push) push_op (l);
      t = l;
    } else {
      static if (has_push) {
        push_op (l);
        push_op (r);
      }
      if (l.y > r.y) {
        _merge (l.right, l.right, r);
        _relax (l);
        t = l;
      } else {
        _merge (r.left, l, r.left);
        _relax (r);
        t = r;
      }
    }
  }

  private static _replace (Node *t, int pos, Value value) {
    static if (has_relax) {
      Node*[128] path = void;
      int n;
    }
    while (true) {
      static if (has_push) push_op (t);
      static if (has_relax) {
        path[n++] = t;
      }
      int ls;
      if (t.left) ls = t.left.sz;
      if (pos == ls) {
        t.value = value;
        break;
      }
      if (pos < ls) {
        t = t.left;
      } else {
        t = t.right;
        pos -= ls + 1;
      }
    }
    static if (has_relax) {
      foreach_reverse (i; 0 .. n) {
        relax_op (path[i]);
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

  private static void _remove (ref Node *t, int pos) in {
    assert (t);
    assert (pos >= 0);
    assert (pos < _size (t));
  } body {
    static if (has_push) push_op (t);
    int ls;
    if (t.left) ls = t.left.sz;
    if (pos == ls) {
      _merge (t, t.left, t.right);
    } else if (pos < ls) {
      _remove (t.left, pos);
      assert (t);
      _relax_dec (t);
    } else {
      _remove (t.right, pos - ls - 1);
      assert (t);
      _relax_dec (t);
    }
  }

  final void insert (int pos, Value value) {
    _insert (root, new Node (value), pos);
  }

  final void remove (int pos) {
    _remove (root, pos);
  }

  final void replace (int pos, Value value) {
    _replace (root, pos, value);
  }

  final Value get (int pos) const in {
    assert (pos < size ());
  } body {
    return _get (root, pos).value;
  }

  final int size () const {
    return _size (root);
  }

  final Node*[] getNodes () {
    Node*[] a;
    a.reserve (_size (root));
    void rec (Node *t) {
      if (t.left) rec (t.left);
      a ~= t;
      if (t.right) rec (t.right);
    }
    rec (root);
    return a;
  }

  final Value[] getValues () {
    Value[] a;
    a.reserve (_size (root));
    void rec (Node *t) {
      static if (has_push) push_op (t);
      if (t.left) rec (t.left);
      a ~= t.value;
      if (t.right) rec (t.right);
    }
    if (root) rec (root);
    return a;
  }

  static if (has_relax) {
    private Node[256] nodes;
    private int cur;

    private void persistent_split (Node *t, ref Node *l, ref Node *r, int key) {
      if (!t) {
        l = r = null;
        return;
      }
      int ls;
      if (t.left) ls = t.left.sz;
      Node *q = &nodes[cur++];
      q.value = t.value;
      q.y = t.y;
      if (key <= ls) {
        q.right = t.right;
        persistent_split (t.left, l, q.left, key);
        r = q;
      } else {
        q.left = t.left;
        persistent_split (t.right, q.right, r, key - ls - 1);
        l = q;
      }
      _relax (q);
    }
    final Extra reduce (int l, int r) {
      cur = 0;
      Node *t1, t2, t3;
      persistent_split (root, t1, t2, l);
      persistent_split (t2, t2, t3, r - l);
      return t2.extra;
    }
  }

  static if (has_update) {
    final void update (int l, int r) {
      if (!l) {
        Node *t1, t2;
        _split (root, t1, t2, r);
        update_op (t1);
        _merge (root, t1, t2);
      } else {
        Node *t1, t2, t3;
        _split (root, t1, t2, l);
        _split (t2, t2, t3, r - l);
        update_op (t2);
        _merge (t2, t2, t3);
        _merge (root, t1, t2);
      }
    }
  }
  this (Value[] v) {
    _build (root, v);
  }
}

unittest {
  import std.range, std.stdio;
  writeln ("Testing ", __FILE__, " ...");
  auto t = new Treap!(int, long, 3);
  t.insert (1, 2);
  auto t2 = new Treap!(int, void, 2);
  t2.insert (1);
  assert (t2.contains (1));
  assert (t2.kthKey (0) == 1);
  assert (t2.countLess (2) == 1);
  assert (t2.countLess (1) == 0);
  assert (t2.remove (1));
  assert (!t2.contains (1));

  auto t0 = new Treap!(int, void, 0);
  t0.insert (1);
  assert (t0.contains (1));
  assert (t0.remove (1));
  assert (!t0.contains (1));

  auto ti = new ImplicitKeyTreap!(int,void)([2,1]);
  assert (ti.get(0)==2);
  assert (ti.get(1)==1);
  assert (equal (ti.getValues(), [2,1]));

}
