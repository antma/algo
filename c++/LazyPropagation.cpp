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
