#include <vector>
#include <cstdint>
#include <cassert>
using namespace std;

class BitSet {
  private:
  vector<uint64_t> a;
  int n;
  int m;
  public:
  BitSet (int _n) {
    n = _n;
    m = (n + 63) >> 6;
    a.assign (m, UINT64_C (0));
  }
  BitSet (const BitSet &bs) : a (bs.a), n (bs.n), m (bs.m) {}
  void add (int k) {
    assert (k >= 0 && k < n);
    a[k>>6] |= (UINT64_C (1)) << (k & 63);
  }
  bool test (int k) const {
    assert (k >= 0 && k < n);
    return (a[k>>6] & (UINT64_C (1)) << (k & 63)) != 0;
  }
  BitSet& operator|= (const BitSet& rhs) {
    for (int i = 0; i < m; ++i) {
      a[i] |= rhs.a[i];
    }
    return *this;
  }
  BitSet& operator&= (const BitSet& rhs) {
    for (int i = 0; i < m; ++i) {
      a[i] &= rhs.a[i];
    }
    return *this;
  }
  bool empty () const {
    for (int i = 0; i < m; ++i) {
      if (a[i]) return false;
    }
    return true;
  }
};
