import std.algorithm, std.conv, std.range;

class DisjointSet {
  private:
    int [] p, h;
    int n;
  public:
  this (int _n) {
    n = _n;
    p = iota (0, n).array;
    h = new int[n];
  }
  int findSet (int x) pure nothrow @nogc {
    if (p[x] == x) {
      return x;
    }
    return p[x] = findSet (p[x]);
  }
  void merge (int i, int j) pure nothrow @nogc {
    i = findSet (i);
    j = findSet (j);
    if (i != j) {
      if (h[i] < h[j]) {
        p[i] = j;
      } else if (h[i] > h[j]) {
        p[j] = i;
      } else {
        p[i] = j;
        ++h[j];
      }
    }
  }
  int biggest_set_size () pure {
    auto c = new int[n];
    foreach (i; 0 .. n) {
      ++c[findSet (i)];
    }
    return c.reduce! (max);
  }
}
class DisjointSetList {
  private:
  int[] p, h, prev, next, maxv;
  int n;
  struct Range {
    int[] _next;
    int begin;
    immutable int end;
    @property bool empty () const pure nothrow @nogc { return begin >= end; }
    @property int front () const pure nothrow @nogc { return begin; }
    void popFront () pure nothrow @nogc { begin = _next[begin]; }
  }
  final int findSet (int x) pure nothrow @nogc {
    if (p[x] == x) {
      return x;
    }
    return p[x] = findSet (p[x]);
  }
  final void join (int x, int y) pure nothrow @nogc {
    x = findSet (x);
    y = findSet (y);
    maxv[x] = maxv[y];
    if (h[x] < h[y]) {
      p[x] = y;
    } else if (h[x] > h[y]) {
      p[y] = x;
    } else {
      p[x] = y;
      ++h[y];
    }
  }
  public:
  this (int _m) pure nothrow {
    n = _m + 1;
    p = iota (0, n).array;
    h = new int[n];
    prev = new int[n];
    next = new int[n];
    maxv = p.dup;
    foreach (i; 0 .. _m) {
      next[i] = i + 1;
      prev[i+1] = i;
    }
    prev[0] = _m;
    next[_m] = 0;
  }
  final void removeKey (int k) pure nothrow @nogc {
    join (k, next[k]);
    int u = prev[k], v = next[k];
    next[u] = v;
    prev[v] = u;
  }
  Range opSlice (size_t begin, size_t end) pure {
    return Range (next, maxv[findSet (begin.to!int)], end.to!int);
  }
};
