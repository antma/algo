//segment tree, median, array prefix with given sum
import std.algorithm;
import std.array;
import std.conv;
import std.math;
import std.range;
import std.stdio;
import std.string;

class SegmentTree(T = int, alias fun) {
  private:
  T [] t;
  T function (const T, const T) op;
  static T* [24] x, y, z;
  size_t n;
  final size_t idx (size_t l, size_t r) const {
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
  public:
  final void update (size_t i, T value) {
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
  const(T) opIndex (size_t k) const { return t[k]; }
  this (T [] a, bool call_build = true) {
    n = a.length;
    t = a;
    t.length = 2 * n;
    if (call_build) {
      build (0, n);
    }
  }
}

//Use case: found array median
class FindKthSegmentTree(T,alias fun) : SegmentTree!(T, fun)
{
  public:
  final size_t find_kth (T k) const
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
  this (T [] a) { super (a); }
}

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
  final void update (size_t i, void delegate(T) func) {
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
  this (T [] a) {
    super (a);
  }
}

struct LongestZeroSegment {
  int zp; // zero prefix length
  int zs; // zero suffix length
  int z;
  int l;  // segment length

  LongestZeroSegment opBinary (string op) (in LongestZeroSegment rhs) const {
    int nl = l + rhs.l;
    int nzp = (zp < l) ? zp : zp + rhs.zp;
    int nzs = (rhs.zs < rhs.l) ? rhs.zs : rhs.zs + zs;
    int nz = max (nzp, nzs, z, rhs.z, zs + rhs.zp);
    return LongestZeroSegment (nzp, nzs, nz, nl);
  }
}

class SegmentTreeSliceUpdate(T = int) {
  private:
  T [] t;
  int n;
  final void build (const T [] a, int v, int l, int r) {
    if (l == r) {
      t[v] = a[l];
    } else {
      immutable m = (l + r) >> 1;
      build (a, v << 1, l, m);
      build (a, (v << 1) + 1, m + 1, r);
    }
  }
  final private void update (int v, int l, int r, int a, int b, T value) {
    if (a <= b) {
      if (l == a && r == b) {
        t[v] += value;
      } else {
        immutable m = (l + r) >> 1;
        immutable w = v << 1;
        update (w, l, m, a, min (m, b), value);
        update (w + 1, m + 1, r, max (m + 1, a), b, value);
      }
    }
  }
  final private T get (int v, size_t l, size_t r, size_t index) const {
    if (l == r) {
      return t[v];
    } else {
      immutable m = (l + r) >> 1;
      T x = t[v];
      x += (index <= m) ? get ((v << 1), l, m, index) : get ((v << 1) + 1, m + 1, r, index);
      return x;
    }
  }
  public:
  final void update (int a, int b, T value) {
    update (1, 0, n - 1, a, b, value);
  }
  final inout(T) opIndex (size_t index) inout {
    return get (1, 0, n - 1, index);
  }
  this (const T [] a) {
    n = a.length.to!(int);
    t = new T[4 * n];
    build (a, 1, 0, n - 1);
  }
}

unittest {
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
}
