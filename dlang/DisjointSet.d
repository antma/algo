import std.algorithm, std.conv, std.range;

final class DisjointSet {
  private:
    int [] p, h;
    int n;
  public:
  this (in int _n) {
    n = _n;
    p = iota (0, n).array;
    h = new int[n];
  }
  pure nothrow @nogc
  int findSet (in int x) {
    if (p[x] == x) {
      return x;
    }
    return p[x] = findSet (p[x]);
  }
  pure nothrow @nogc
  bool merge (int i, int j) {
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
      return true;
    }
    return false;
  }
  pure
  int biggestSetSize () {
    auto c = new int[n];
    foreach (i; 0 .. n) {
      ++c[findSet (i)];
    }
    return c.reduce! (max);
  }
}

final class DisjointSetList {
  private:
  int[] p, h, prev, next, maxv;
  int n;
  struct Range {
    int[] _next;
    int begin;
    immutable int end;
    @property pure nothrow @nogc
    bool empty () const { return begin >= end; }
    @property pure nothrow @nogc
    int front () const { return begin; }
    pure nothrow @nogc
    void popFront () { begin = _next[begin]; }
  }
  pure nothrow @nogc
  int findSet (int x) {
    if (p[x] == x) {
      return x;
    }
    return p[x] = findSet (p[x]);
  }
  pure nothrow @nogc
  void join (int x, int y) {
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
  pure nothrow
  this (int _m) {
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
  pure nothrow @nogc
  void removeKey (int k) {
    join (k, next[k]);
    int u = prev[k], v = next[k];
    next[u] = v;
    prev[v] = u;
  }
  pure
  Range opSlice (size_t begin, size_t end) {
    return Range (next, maxv[findSet (begin.to!int)], end.to!int);
  }
}
