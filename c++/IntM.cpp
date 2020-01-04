#include <cstdint>

using namespace std;

struct IntM {
  static constexpr uint32_t m = 1000000007U;
  uint32_t v;
  IntM (const uint32_t x = 0) : v (x) {}
};
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
