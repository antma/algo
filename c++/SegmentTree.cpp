#include <cstdint>
#include <vector>
#include <algorithm>
using namespace std;

template <typename T, class O> class SegmentTree {
  private:
  size_t n;
  vector<T> t;
  const T zero;
  const O &op;
  void build () {
    for (auto i = n - 1; i >= 1; --i) {
      const auto k = i << 1;
      t[i] = op (t[k], t[k+1]);
    }
  }
  public:
  void update (size_t p, const T &v) {
    for (t[p += n] = v; p > 1; p >>= 1) {
      t[p>>1] = op (t[p], t[p ^ 1]);
    }
  }
  T reduce (size_t l, size_t r) const {
    T res (zero);
    for (l += n, r += n; l < r; l >>= 1, r >>= 1) {
      if (l & 1) {
        res = op (res, t[l++]);
      }
      if (r & 1) {
        res = op (t[--r], res);
      }
    }
    return res;
  }
  SegmentTree (const vector<T> &a, const T &_zero, const O &o) : n (a.size ()), t (2 * n), zero (_zero), op (o) {
    copy (a.cbegin (), a.cend (), t.begin () + n);
    build ();
  }
};
