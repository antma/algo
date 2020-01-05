#include <cstdint>
#include <cassert>
#include <iostream>

using namespace std;

int gcdExt (int a, int b, int &x, int &y) {
  if (!b) {
    x = 1;
    y = 0;
    return a;
  }
  auto res = gcdExt (b, a % b, y, x);
  y -= x * (a / b);
  return res;
}

struct IntM {
  static constexpr uint32_t m = 1000000007U;
  uint32_t v;
  IntM (const uint32_t x = 0) : v (x) {}
  IntM inversion () const {
    int x, y, g = gcdExt (m, v, x, y);
    assert (g == 1);
    if (y < 0) y += m;
    return IntM (y);
  }
};
ostream& operator<<(ostream &o, const IntM x) { return o << x.v; }
IntM operator+ (const IntM x, const IntM y) {
  uint32_t r = x.v + y.v;
  if (r >= IntM::m) r -= IntM::m;
  return IntM (r);
}
IntM operator- (const IntM x, const IntM y) {
  uint32_t r = x.v - y.v;
  if (x.v < y.v) r += IntM::m;
  return IntM (r);
}
IntM operator* (const IntM x, const IntM y) {
  return IntM (static_cast<uint32_t> ((static_cast<uint64_t> (x.v) * y.v) % IntM::m));
}
