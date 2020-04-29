import std.array;
import core.bitop;

class LazyPropagationSegmentTree(T = int, D = int, D init = D.init) {
  immutable size_t n;
  immutable int h;
  T[] t;
  D[] d;

  abstract void calc (size_t p, int k);
  abstract void apply (size_t p, D value, int k);

  final void build (size_t l, size_t r) {
    int k = 2;
    for (l += n, r += n - 1; l > 1; k <<= 1) {
      l >>= 1;
      r >>= 1;
      foreach_reverse (i; l .. r + 1) {
        calc (i, k);
      }
    }
  }

  final void push (size_t l, size_t r) {
    int s = h, k = 1 << (h-1);
    for (l += n, r += n - 1; s > 0; --s, k >>= 1) {
      foreach (i; l >> s .. (r >> s) + 1) {
        immutable delta = d[i];
        if (delta != init) {
          apply (i << 1, delta, k);
          apply ((i << 1) | 1, delta, k);
          d[i] = init;
        }
      }
    }
  }

  this (const T[] a) {
    n = a.length;
    h = bsr (n);
    t = uninitializedArray!(T[])(n);
    t ~= a;
    d = uninitializedArray!(D[])(n);
    d[] = init;
    build (0, n);
  }
}

class AssignSumLazyPropagationSegmentTree (T = int) : LazyPropagationSegmentTree!(T, T, T.min) {
  override void calc (size_t p, int k) {
    if (d[p] == T.min) t[p] = t[p<<1] + t[(p << 1) | 1];
    else t[p] = d[p] * k;
  }

  override void apply (size_t p, T value, int k) {
    t[p] = value * k;
    if (p < n) d[p] = value;
  }

  final void assign (size_t l, size_t r, T value) {
    debug stderr.writefln ("assign (l:%d, r:%d, value:%d)", l, r, value);
    push (l, l + 1);
    push (r - 1, r);
    immutable l0 = l, r0 = r;
    int k = 1;
    for (l += n, r += n; l < r; l >>= 1, r >>= 1, k <<= 1) {
      if (l & 1) {
        apply (l++, value, k);
      }
      if (r & 1) {
        apply (--r, value, k);
      }
    }
    build (l0, l0 + 1);
    build (r0 - 1, r0);
  }

  final T sum (size_t l, size_t r) {
    push (l, l + 1);
    push (r - 1, r);
    T res;
    for (l += n, r += n; l < r; l >>= 1, r >>= 1) {
      if (l & 1) {
        res += t[l++];
      }
      if (r & 1) {
        res += t[--r];
      }
    }
    return res;
  }
  this (T[] a) {
    super (a);
  }
}

final class IncrementSumLazyPropagationSegmentTree(T = int) {
  immutable size_t n;
  immutable int h;
  T[] t, d;

  void calc (size_t p, int k) {
    t[p] = t[p<<1] + t[(p<<1) | 1] + k * d[p];
  }

  void apply (size_t p, T value, int k) {
    t[p] += value * k;
    if (p < n) d[p] += value;
  }

  void build (size_t l, size_t r) {
    int k = 2;
    for (l += n, r += n - 1; l > 1; k <<= 1) {
      l >>= 1;
      r >>= 1;
      foreach_reverse (i; l .. r + 1) {
        calc (i, k);
      }
    }
  }

  void push (size_t l, size_t r) {
    int s = h, k = 1 << (h-1);
    for (l += n, r += n - 1; s > 0; --s, k >>= 1) {
      foreach (i; l >> s .. (r >> s) + 1) {
        immutable delta = d[i];
        if (delta) {
          apply (i << 1, delta, k);
          apply ((i << 1) | 1, delta, k);
          d[i] = 0;
        }
      }
    }
  }

  void increment (size_t l, size_t r, T value) {
    if (!value) return;
    push (l, l + 1);
    push (r - 1, r);
    size_t l0 = l, r0 = r;
    int k = 1;
    for (l += n, r += n; l < r; l >>= 1, r >>= 1, k <<= 1) {
      if (l & 1) apply (l++, value, k);
      if (r & 1) apply (--r, value, k);
    }
    build (l0, l0 + 1);
    build (r0 - 1, r0);
  }
  T sum (size_t l, size_t r) {
    push (l, l + 1);
    push (r - 1, r);
    T res;
    for (l += n, r += n; l < r; l >>= 1, r >>= 1) {
      if (l & 1) res += t[l++];
      if (r & 1) res += t[--r];
    }
    return res;
  }

  this (size_t _n) {
    n = _n;
    h = bsr (n);
    t = new T[](2 * n);
    d = new T[n];
  }

  this (const T[] a) {
    n = a.length;
    h = bsr (n);
    t = uninitializedArray!(T[])(n);
    t ~= a;
    d = new T[n];
    build (0, n);
  }
}

unittest {
  import std.algorithm;
  import std.stdio;
  writeln ("Testing ", __FILE__, " ...");
  int[] a = [ 1, 2, 3, 4, 5];
  auto t = new AssignSumLazyPropagationSegmentTree!int (a);
  assert (t.sum (0, a.length) == a.sum);
  t.assign (1, 3, 10);
  a[1] = a[2] = 10;
  assert (t.sum (0, a.length) == a.sum);
  auto t2 = new IncrementSumLazyPropagationSegmentTree!int (a);
  assert (t2.sum (0, a.length) == a.sum);
  t2.increment (1, 3, 3);
  a[1 .. 3] += 3;
  assert (t2.sum (0, a.length) == a.sum);
}
