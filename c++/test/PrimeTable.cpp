#include <cstdint>
#include <cassert>
#include "../PrimeTable.cpp"

int main (void) {
  PrimeTable p (1000000);
  uint64_t s = 0;
  vector<int> a;
  p.getPrimes (a);
  for (const auto i : a) s += i;
  assert (s == UINT64_C(37550402023));
}
