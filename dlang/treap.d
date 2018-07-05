import std.random;

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
