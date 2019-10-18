import std.algorithm, std.conv, std.range;
import std.random;
import std.traits;

struct TreapNode(Key, Value = void, Extra=void, bool has_size = false) {
  enum has_value = !(is (Value == void));
  enum has_extra = !(is (Extra == void));
  TreapNode!(Key, Value, Extra, has_size)* left, right;
  Key x;
  static if (has_value) Value value;
  static if (has_extra) Extra extra;
  int y;
  static if (has_size) int sz;
}

final class Treap(Key, Value = void, Extra = void, bool has_size = false, alias relax_op="") {
  private:
  enum has_relax = isCallable!relax_op;
  alias Node = TreapNode!(Key, Value, Extra, has_size);
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
    static if (has_size) p.sz = 1;
    return p;
  }
  static if (Node.has_value) {
    static Node* newNode (Key key, Value value) {
      Node *p = allocNode ();
      p.x = key;
      p.y = uniform!int;
      p.value = value;
      static if (has_relax) relax_op (p);
      return p;
    }
  } else {
    static Node* newNode (Key key) {
      Node *p = allocNode ();
      p.x = key;
      p.y = uniform!int;
      static if (has_relax) relax_op (p);
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
  static if (has_size) {
    static int _size (const Node *t) {
      return t ? t.sz : 0;
    }
    static void _relax (Node *t) {
      t.sz = 1;
      if (t.left) t.sz += t.left.sz;
      if (t.right) t.sz += t.right.sz;
      static if (has_relax) relax_op (t);
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
    static const(Node) *_kthNode (const(Node) *t, size_t k) {
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
    static if (has_size) _relax (t);
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
      static if (has_size) ++t.sz;
      static if (has_relax) relax_op (t);
      return t;
    }
    _split (t, p.x, p.left, p.right);
    static if (has_size) _relax (p);
    return p;
  }

  static Node *_merge (Node *l, Node *r) {
    if (!l) {
      return r;
    } else if (!r) {
      return l;
    } else if (l.y > r.y) {
      l.right = _merge (l.right, r);
      static if (has_size) _relax (l);
      return l;
    } else {
      r.left = _merge (l, r.left);
      static if (has_size) _relax (r);
      return r;
    }
  }

  static bool _remove (ref Node *t, Key x) {
    if (t.x == x) {
      Node *w = _merge (t.left, t.right), u = free_nodes.left, v = &free_nodes;
      u.right = t; t.left = u;
      t.right = v; v.left = t;
      t = w;
      return true;
    } if (x < t.x) {
      if (!t.left || !_remove (t.left, x)) return false;
      static if (has_size) --t.sz;
      static if (has_relax) relax_op (t);
      return true;
    } else {
      if (!t.right || !_remove (t.right, x)) return false;
      static if (has_size) --t.sz;
      static if (has_relax) relax_op (t);
      return true;
    }
  }

  public:

  static if (Node.has_value) {
    void insert (Key key, Value value) {
      root = _insert (root, newNode (key, value));
    }
  } else {
    void insert (Key key) {
      root = _insert (root, newNode (key));
    }
  }

  static if (has_size) {
    size_t countLess (Key x) { return _countLess (root, x); }
    Key kthKey (size_t k) const {
      auto p = _kthNode (root, k);
      assert (p);
      return p.x;
    }
  }

  bool remove (Key x) {
    if (!root) return false;
    return _remove (root, x);
  }

  void clear () {
    _clear (root);
    root = null;
  }

  bool contains (Key x) { return _find (root, x) !is null; }
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

final class ImplicitKeyTreap(Value, Extra=void, alias relax_op="", alias push_op="", alias update_op="") {
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

  void insert (int pos, Value value) {
    _insert (root, new Node (value), pos);
  }

  void remove (int pos) {
    _remove (root, pos);
  }

  void replace (int pos, Value value) {
    _replace (root, pos, value);
  }

  Value get (int pos) const in {
    assert (pos < size ());
  } body {
    return _get (root, pos).value;
  }

  int size () const {
    return _size (root);
  }

  Node*[] getNodes () {
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

  Value[] getValues () {
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
    Extra reduce (int l, int r) {
      cur = 0;
      Node *t1, t2, t3;
      persistent_split (root, t1, t2, l);
      persistent_split (t2, t2, t3, r - l);
      return t2.extra;
    }
  }

  static if (has_update) {
    void update (int l, int r) {
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
  import std.range, std.stdio, std.format;
  writeln ("Testing ", __FILE__, " ...");
  import std.format;
  auto t = new Treap!(int, void, long, true, function void(TreapNode!(int, void, long, true) *t) {
    t.extra = t.x;
    if (t.left) t.extra += t.left.extra;
    if (t.right) t.extra += t.right.extra;
  });
  static assert (t.has_relax);
  rndGen.seed (1);
  int[] c;
  void check (lazy string msg) {
    assert (t.root.extra == sum (c), format ("SUM: tread %d, slow %d, c = %s, %s", t.root.extra, sum (c), c, msg));
  }
  foreach (k; 0 .. 100) {
    int i = uniform (0, k + 1);
    c ~= i;
    t.insert (i);
    check (format ("k = %d", k));
  }
  foreach (k; 1 .. c.length) {
    auto i = uniform (0, c.length);
    swap (c[i], c[$-1]);
    t.remove (c[$-1]);
    --c.length;
    check (format ("k = %d", k));
  }
}
