#include <vector>
#include <limits>

using namespace std;

template<typename T> class LazyPropagationMinIncrement {
  const int n, h;
  vector<T> t, d;
  void apply (int p, T value) {
    t[p] += value;
    if (p < n) {
      d[p] += value;
    }
  }
  void build (int p) {
    while (p > 1) {
      p >>= 1;
      t[p] = min (t[p << 1], t[(p << 1) | 1]) + d[p];
    }
  }
  void push (int p) {
    for (int s = h; s > 0; --s) {
      int i = p >> s;
      if (d[i]) {
        apply (i << 1, d[i]);
        apply ((i << 1) | 1, d[i]);
        d[i] = 0;
      }
    }
  }
  public:
  void increment (int l, int r, T value) {
    const int l0 = l += n, r0 = r += n;
    for (; l < r; l >>= 1, r >>= 1) {
      if (l & 1) {
        apply (l++, value);
      }
      if (r & 1) {
        apply (--r, value);
      }
    }
    build (l0);
    build (r0 - 1);
  }
  T query (int l, int r) {
    push (l += n);
    r += n;
    push (r - 1);
    T res = numeric_limits<T>::max();
    for (; l < r; l >>= 1, r >>= 1) {
      if (l & 1) {
        res = min (res, t[l++]);
      }
      if (r & 1) {
        res = min (t[--r], res);
      }
    }
    return res;
  }
  LazyPropagationMinIncrement (int _n) :
    n (_n), h (static_cast<int> (sizeof(int) * 8 - __builtin_clz (n))),
    t (2 * n, T (0)),
    d (n, T (0))
  {}
};

template<typename T> class LazyPropagationSumIncrement {
  const int n, h;
  vector<T> t, d;

  void calc (int p, int k) {
    t[p] = t[p<<1] + t[(p<<1) | 1] + k * d[p];
  }

  void apply (int p, T value, int k) {
    t[p] += value * k;
    if (p < n) d[p] += value;
  }

  void build (int l, int r) {
    int k = 2;
    for (l += n, r += n - 1; l > 1; k <<= 1) {
      l >>= 1;
      r >>= 1;
      for (int i = r; i >= l; --i) calc (i, k);
    }
  }

  void push (int l, int r) {
    int s = h, k = 1 << (h-1);
    for (l += n, r += n - 1; s > 0; --s, k >>= 1) {
      for (int i = l >> s; i <= r >> s; ++i) if (d[i] != 0) {
        apply (i<<1, d[i], k);
        apply ((i<<1) | 1, d[i], k);
        d[i] = 0;
      }
    }
  }

  public:
  void increment (int l, int r, T value) {
    if (value == 0) return;
    push (l, l + 1);
    push (r - 1, r);
    int l0 = l, r0 = r, k = 1;
    for (l += n, r += n; l < r; l >>= 1, r >>= 1, k <<= 1) {
      if (l & 1) apply (l++, value, k);
      if (r & 1) apply (--r, value, k);
    }
    build (l0, l0 + 1);
    build (r0 - 1, r0);
  }
  T sum (int l, int r) {
    push (l, l + 1);
    push (r - 1, r);
    T res = 0;
    for (l += n, r += n; l < r; l >>= 1, r >>= 1) {
      if (l & 1) res += t[l++];
      if (r & 1) res += t[--r];
    }
    return res;
  }
  LazyPropagationSumIncrement (int _n) :
    n (_n), h (static_cast<int> (sizeof(int) * 8 - __builtin_clz (n))),
    t (2 * n, T (0)),
    d (n, T (0))
  {}
};
