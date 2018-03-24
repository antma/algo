//segment tree, median, array prefix with given sum
import std.algorithm;
import std.array;
import std.conv;
import std.math;
import std.range;
import std.stdio;
import std.string;

class SegmentTree {
  private int [] t;
  private int n;
  final int func (int x, int y) {
    return x + y;
  }
  final void build (int [] a, int v, int l, int r) {
    if (l == r) {
      t[l] = a[l];
    } else {
      immutable m = (l + r) >> 1;
      build (a, v << 1, l, m);
      build (a, (v << 1) + 1, m + 1, r);
      t[v] = func (t[v << 1], t[(v << 1) + 1]);
    }
  }
  final void update (int i, int new_value) {
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
      t[v >> 1] = func (t[v], t[v ^ 1]);
      v >>= 1;
    }
  }
  final int find_kth (int k) const 
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
  this (int [] a) {
    n = a.length.to!(int);
    t = new int[4 * n];
    build (a, 1, 0, n - 1);
  }
};
