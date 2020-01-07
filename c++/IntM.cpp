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
  IntM () {}
  IntM (const uint32_t x) : v (x) {}
  IntM& operator+= (const IntM x) & {
    v += x.v;
    if (v >= m) v -= m;
    return *this;
  }
  IntM& operator-= (const IntM x) & {
    if (v < x.v) v += m;
    v -= x.v;
    return *this;
  }
  IntM& operator*= (const IntM x) & {
    v = static_cast<uint32_t> ((static_cast<uint64_t> (v) * x.v) % m);
    return *this;
  }
  auto operator+ (const IntM x) const {
    uint32_t r = v + x.v;
    if (r >= m) r -= m;
    return IntM (r);
  }
  auto operator- (const IntM x) const {
    uint32_t r = v - x.v;
    if (v < x.v) r += m;
    return IntM (r);
  }
  auto operator* (const IntM x) const { return IntM (static_cast<uint32_t> ((static_cast<uint64_t> (v) * x.v) % m)); }
  IntM inversion () const {
    int x, y, g = gcdExt (m, v, x, y);
    assert (g == 1);
    if (y < 0) y += m;
    return IntM (y);
  }
};
ostream& operator<<(ostream &o, const IntM x) { return o << x.v; }
