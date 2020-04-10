#include <vector>
using namespace std;
template<typename T> class FenwickTree {
  vector<T> a;
  public:
  FenwickTree (const int n) : a (n, (T) 0) {}
  void update (const int x, const T val) {
    const int n = (int) a.size ();
    for (int i = x; i < n; i |= i + 1) {
      a[i] += val;
    }
  }
  T reduce (const int x) const {
    T r = 0;
    for (int i = x; i >= 0; i = (i & (i + 1)) - 1) {
      r += a[i];
    }
    return r;
  }
};
