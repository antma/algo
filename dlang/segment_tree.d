//segment tree, median, array prefix with given sum
import std.algorithm;
import std.array;
import std.conv;
import std.math;
import std.range;
import std.stdio;
import std.string;

class SegmentTree(T = int) {
  private T [] t;
  private int n;
  private T zero_value;
  final void build (T [] a, int v, int l, int r) {
    if (l == r) {
      t[v] = a[l];
    } else {
      immutable m = (l + r) >> 1;
      build (a, v << 1, l, m);
      build (a, (v << 1) + 1, m + 1, r);
      t[v] = t[v << 1] + t[(v << 1) + 1];
    }
  }
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
      t[v >> 1] = t[v] + t[v ^ 1];
      v >>= 1;
    }
  }
  final T reduce (int v, int l, int r, int a, int b) {
    if (a > b) {
      return zero_value;
    }
    if (a == l && b == r) {
      return t[v];
    }
    immutable int m = (l + r) >> 1;
    v <<= 1;
    return reduce (v, l, m, a, min (b, m)) + reduce (v + 1, m + 1, r, max (a, m+1), b);
  }
  final T reduce (int a, int b) { return reduce (1, 0, n - 1, a, b); }
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
  this (T [] a, T zero_value_ = T.init) {
    n = a.length.to!(int);
    t = new T[4 * n];
    zero_value = zero_value_;
    build (a, 1, 0, n - 1);
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

unittest {
  writeln ("Testing segment_tree.d ...");
  auto st = new SegmentTree!long ([1L, 2L]); 
  assert (st.reduce (0, 1) == 3L);
}
