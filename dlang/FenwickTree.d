class FenwickTree(T = int, alias op = (x, y) => x + y, T zero = T.init) {
  private:
  T [] a;
  size_t n;
  public:
  this (size_t _n) pure nothrow {
    n = _n;
    a = new int[n];
    static if (zero != T.init) a[] = zero;
  }
  void update (size_t x, T v) pure nothrow @nogc in {
    assert (x < n);
  } body {
    for (auto i = x; i < n; i |= i + 1) {
      a[i] = op (a[i], v);
    }
  }
  //returns sum[0..x+1]
  T reduce (size_t x) const pure nothrow @nogc in {
    assert (x < n);
  } body {
    T r = zero;
    for (auto i = x; i != size_t.max; i = (i & (i + 1)) - 1) {
      r = op (r, a[i]);
    }
    return r;
  }
}

unittest {
  import std.stdio;
  writeln ("Testing fenwick_tree.d ...");
  auto f = new FenwickTree!int(5);
  f.update (0, 1);
  assert (f.reduce (1) == 1);
  f.update (3, 1);
  assert (f.reduce (3) == 2);
  f.update (1, -1);
  assert (f.reduce (3) == 1);
}
