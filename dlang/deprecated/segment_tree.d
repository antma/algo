import std.algorithm, std.conv;

class SegmentTree(T = int, alias fun) {
  private:
  T [] t;
  size_t n;
  final size_t idx (size_t l, size_t r) const pure nothrow @nogc {
    return (l + 1 == r) ? l : n + ((l + r) >> 1);
  }
  //[l, r)
  final size_t build (size_t l, size_t r) {
    if (r - l > 1) {
      immutable m = (l + r) >> 1, a = build (l, m), b = build (m, r), c = n + m;
      t[c] = fun (t[a], t[b]);
      return c;
    } else {
      return l;
    }
  }
  final const(T) reduce (size_t l, size_t r, size_t a, size_t b) const {
    while (a != l || b != r) {
      immutable m = (l + r) >> 1;
      if (m <= a) {
        l = m;
      } else if (m >= b) {
        r = m;
      } else {
        return fun (reduce (l, m, a, m), reduce (m, r, m, b));
      }
    }
    return t[(l + 1 == r) ? l : n + ((l + r) >> 1)];
  }
  this (T [] t_, size_t n_) { t = t_; n = n_; }
  public:
  final void update (size_t i, T value) {
    T* [32] x = void, y = void, z = void;
    int k = 0;
    size_t l = 0, r = n;
    auto m = (l + r) >> 1;
    while (m - l > 2) {
      x[k] = &t[n + m];
      immutable m1 = (l + m) >> 1;
      y[k] = &t[n + m1];
      immutable m2 = (m + r) >> 1;
      z[k++] = &t[n + m2];
      if (i < m) {
        r = m;
        m = m1;
      } else {
        l = m;
        m = m2;
      }
    }
    while (m != l) {
      x[k] = &t[n + m];
      immutable m1 = (l + m) >> 1;
      y[k] = &t[m1 + ((l != m1) ? n : 0)];
      immutable m2 = (m + r) >> 1;
      z[k++] = &t[m2 + ((m2 != m) ? n : 0)];
      if (i < m) {
        r = m;
        m = m1;
      } else {
        l = m;
        m = m2;
      }
    }
    t[i] = value;
    while (--k >= 0) {
      *x[k] = fun (*y[k], *z[k]);
    }
  }
  final const(T) reduce (size_t a, size_t b) const { return reduce (0, n, a, b); }
  const(T) opIndex (size_t k) const pure nothrow @nogc { return t[k]; }
  final auto merge (const this rhs) const pure nothrow in {
    assert (n == rhs.n);
  } body {
    auto u = new T[t.length];
    foreach (i; 0 .. t.length) {
      u[i] = fun (t[i], rhs.t[i]);
    }
    return new SegmentTree! (T, fun) (u, n);
  }
  this (T [] a) pure nothrow {
    n = a.length;
    t = a;
    t.length = 2 * n;
    build (0, n);
  }

}

//Use case: found array median
class FindKthSegmentTree(T, alias fun) : SegmentTree!(T, fun) {
  public:
  final size_t find_kth (T k) const pure nothrow @nogc
  in  {
    assert (k >= 0);
  } body {
    size_t l = 0, r = n;
    while (true) {
      if (k >= t[idx (l, r)]) {
        return -1;
      }
      if (l + 1 == r) {
        return l;
      }
      immutable m = (l + r) >> 1;
      immutable v = t[idx (l, m)];
      if (v > k) {
        r = m;
      } else {
        k -= v;
        l = m;
      }
    }
  }
  this (T [] a) pure nothrow { super (a); }
}

//WARNING: update isn't worked for addition operation (only for max on rectangle)
class SegmentTree2D(T, U, alias fun, alias fun_reduce) : SegmentTree!(T, fun) {
  private:
  static U delegate(const T) extractor;
  final U reduce2d (size_t l, size_t r, size_t a, size_t b) const {
    while (a != l || b != r) {
      immutable m = (l + r) >> 1;
      if (m <= a) {
        l = m;
      } else if (m >= b) {
        r = m;
      } else {
        return fun_reduce (reduce2d (l, m, a, m), reduce2d (m, r, m, b));
      }
    }
    return extractor (t[(l + 1 == r) ? l : n + ((l + r) >> 1)]);
  }
  public:
  final U reduce2d (size_t a, size_t b, U delegate(const T) extractor_) const
  in {
    assert (b <= n);
    assert (a < b);
  } body {
    extractor = extractor_;
    return reduce2d (0, n, a, b);
  }
  final void each (size_t i, void delegate(T) func) {
    T* [32] x = void;
    int k = 0;
    size_t l = 0, r = n;
    while (l + 1 != r) {
      immutable m = (l + r) >> 1;
      x[k++] = &t[n + m];
      ((i < m) ? r : l) = m;
    }
    func (t[i]);
    while (--k >= 0) {
      func (*x[k]);
    }
  }
  this (T [] a) pure nothrow {
    super (a);
  }
}

struct LongestZeroSegment {
  int zp; // zero prefix length
  int zs; // zero suffix length
  int z;
  int l;  // segment length

  LongestZeroSegment opBinary (string op) (in LongestZeroSegment rhs) const pure nothrow @nogc {
    int nl = l + rhs.l;
    int nzp = (zp < l) ? zp : zp + rhs.zp;
    int nzs = (rhs.zs < rhs.l) ? rhs.zs : rhs.zs + zs;
    int nz = max (nzp, nzs, z, rhs.z, zs + rhs.zp);
    return LongestZeroSegment (nzp, nzs, nz, nl);
  }
}

class SegmentTreeSliceUpdate(T = int, alias fun) {
  private:
  T [] t;
  size_t n;
  final size_t idx (size_t l, size_t r) const pure nothrow @nogc {
    return (l + 1 == r) ? l : n + ((l + r) >> 1);
  }
  final void update (size_t l, size_t r, size_t a, size_t b, T value) pure nothrow @nogc {
    if (a < b) {
      if (l == a && r == b) {
        size_t k = idx (l, r);
        t[k] = fun (t[k], value);
      } else {
        immutable m = (l + r) >> 1;
        update (l, m, a, min (m, b), value);
        update (m, r, max (m, a), b, value);
      }
    }
  }
  final T get (size_t l, size_t r, size_t k) const pure nothrow @nogc {
    if (r - l > 1) {
      immutable m = (l + r) >> 1;
      return fun (t[n + m], (k < m) ? get (l, m, k) : get (m, r, k));
    } else {
      return t[l];
    }
  }
  public:
  final void update (size_t a, size_t b, T value) pure nothrow @nogc {
    update (0, n, a, b, value);
  }
  final inout(T) opIndex (size_t index) inout pure nothrow @nogc {
    return get (0, n, index);
  }
  this (const T [] a) pure nothrow {
    n = a.length;
    t = new T[2 * n];
    t[0 .. n] = a[0 .. n];
  }
}

unittest {
  import std.stdio, std.conv, std.random, std.range;
  writeln ("Testing segment_tree.d ...");
  auto st = new SegmentTree!(long,(x, y) => x + y) ([1L, 2L]);
  assert (st.reduce (0, 2) == 3L);
  auto a = new LongestZeroSegment[5];
  foreach (i; 0 .. a.length) {
    a[i].l = 1;
  }
  auto st2 = new SegmentTree!(LongestZeroSegment, (x, y) => x + y)(a);
  st2.update (4, LongestZeroSegment (1, 1, 1, 1));
  assert (st2.reduce (0, a.length.to!(int)).z == 1);
  st2.update (3, LongestZeroSegment (1, 1, 1, 1));
  assert (st2.reduce (0, a.length.to!(int)).z == 2);
  st2.update (2, LongestZeroSegment (1, 1, 1, 1));
  assert (st2.reduce (0, a.length.to!(int)).z == 3);

  auto st3 = new SegmentTreeSliceUpdate!(int, (x, y) => x + y) ( [0, 0, 0, 0]);
  st3.update (1, 3, 1);
  assert (st3[0] == 0);
  assert (st3[1] == 1);
  assert (st3[2] == 1);
  assert (st3[3] == 0);

  void test2d (const int h, const int w, const int m) {
    auto c = new int[][] (h, w);
    assert (c.length == h);
    assert (c[0].length == w);
    Mt19937 gen;
    gen.seed (12345);
    foreach (ref row; c) {
      foreach (ref x; row) {
        x = uniform (-m, m+1, gen);
      }
    }

    auto add = function (int a, int b) { return max (a, b); };
    auto t = new SegmentTree! (int, add) [h];
    foreach (i, row; c) {
      assert (row.length == w);
      t[i] = new SegmentTree! (int, add) (row);
    }

    foreach (changes; 0 .. 100) {
      immutable j = uniform (0, h, gen);
      immutable i = uniform (0, w, gen);
      c[j][i] = uniform (-m, m+1, gen);
      t[j].update (i, c[j][i]);
      assert (c[j].reduce! (add) == t[j].reduce (0, w));
    }

    auto st2d = new SegmentTree2D! (SegmentTree! (int, add), int, (a, b) => a.merge (b), add) (t);
    foreach (j1; 0 .. h) foreach (j2; j1 + 1 .. h + 1) foreach (i1; 0 .. w) foreach (i2; i1 + 1 .. w + 1) {
      immutable v2 = iota (j1, j2).map! (j => c[j][i1 .. i2].reduce! (add)).reduce! (add);
      immutable v1 = st2d.reduce2d (j1, j2, t => t.reduce (i1, i2));
      assert (v1 == v2);
    }

    foreach (changes; 0 .. 100) {
      immutable v2 = iota (0, h).map! (j => c[j].reduce! (add)).reduce! (add);
      immutable v1 = st2d.reduce2d (0, h, t => t.reduce (0, w));
      writeln (v2, " ", v1);
      assert (v1 == v2);
      immutable j = uniform (0, h, gen);
      immutable i = uniform (0, w, gen);
      c[j][i] = uniform (-m, m+1, gen);
      st2d.each (j, t => t.update (i, c[j][i]));
    }
  }
  //test2d (5, 10, 10000);
}

