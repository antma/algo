#include <vector>
using namespace std;
template<typename T, class O> class FenwickTree {
  private:
  vector<T> a;
  const O o;
  const T z;
  const int m;
  public:
  FenwickTree (const int n, const O &op, const T zero) : a (n, zero), o (op), z (zero), m (n) {}
  void update (const int x, const T val) {
    for (int i = x; i < m; i |= i + 1) {
      a[i] = o (a[i], val);
    }
  }
  T reduce (const int x) const {
    T r = z;
    for (int i = x; i >= 0; i = (i & (i + 1)) - 1) {
      r = o (a[i], r);
    }
    return r;
  }
};
