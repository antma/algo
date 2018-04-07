//segment tree, median, array prefix with given sum
import std.algorithm;
import std.array;
import std.conv;
import std.math;
import std.range;
import std.stdio;
import std.string;

class SegmentTree(T = int) {
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
      t[v] = t[v << 1] + t[(v << 1) + 1];
    }
  }
  final T reduce (int v, int l, int r, int a, int b) {
    if (a == l && b == r) {
      return t[v];
    }
    immutable int m = (l + r) >> 1, x = min (b, m), y = max (a, m + 1);
    v <<= 1;
    if (a <= x) {
      if (y <= b) {
        return reduce (v, l, m, a, x) + reduce (v + 1, m + 1, r, y, b);
      } else {
        return reduce (v, l, m, a, x);
      }
    } else {
      assert (y <= b);
      return reduce (v + 1, m + 1, r, y, b);
    }
  }
  public:
  final void update (int i, T new_value) {
    int l = 0, r = n - 1, v = 1;
    while (l < r) {
      immutable m = (l + r) >> 1;
      v <<= 1;
      if (i <= m) {
        r = m;
      } else {
        ++v;
        l = m + 1;
      }
    }
    t[v] = new_value;
    while (v > 1) {
      v &= ~1;
      t[v >> 1] = t[v] + t[v + 1];
      v >>= 1;
    }
  }
  final T reduce (int a, int b) { return reduce (1, 0, n - 1, a, b); }
  this (const T [] a) {
    n = a.length.to!(int);
    t = new T[4 * n];
    build (a, 1, 0, n - 1);
  }
}

//Use case: found array median
class FindKthSegmentTree(T) : SegmentTree!T
{
  public:
  final int find_kth (T k) const
  in  {
    assert (k >= 0);
  } body {
    int l = 0, r = n - 1, v = 1;
    while (true) {
      if (k >= t[v]) {
        return -1;
      }
      if (l == r) {
        return l;
      }
      immutable m = (l + r) >> 1;
      v <<= 1;
      if (t[v] > k) {
        r = m;
      } else {
        k -= t[v++];
        l = m + 1;
      }
    }
  }
  this (T [] a) { super (a); }
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
  auto st = new SegmentTree!long ([1L, 2L]);
  assert (st.reduce (0, 1) == 3L);
  auto a = new LongestZeroSegment[5];
  foreach (i; 0 .. a.length) {
    a[i].l = 1;
  }
  auto st2 = new SegmentTree!LongestZeroSegment (a);
  st2.update (4, LongestZeroSegment (1, 1, 1, 1));
  assert (st2.reduce (0, a.length.to!(int) - 1).z == 1);
  st2.update (3, LongestZeroSegment (1, 1, 1, 1));
  assert (st2.reduce (0, a.length.to!(int) - 1).z == 2);
  st2.update (2, LongestZeroSegment (1, 1, 1, 1));
  assert (st2.reduce (0, a.length.to!(int) - 1).z == 3);
}
