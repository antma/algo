//Origin: https://codeforces.com/blog/entry/17507
#include <algorithm>
#include <cstdint>
#include <ctime>
#include <random>
#include <utility>
#include <vector>

using namespace std;

struct HashT {
  static constexpr uint32_t p1 = 4294967291U;
  static constexpr uint32_t p2 = 4294967279U;
  uint32_t r1, r2;
  static inline uint32_t addm (const uint32_t x, const uint32_t y, const uint32_t m) {
    uint32_t r = x + y;
    return (r < x || r >= m) ? (r - m) : r;
  }
  static inline uint32_t subm (const uint32_t x, const uint32_t y, const uint32_t m) {
    uint32_t r = x - y;
    return (x < y) ? (r + m) : r;
  }
  static inline uint32_t mulm (const uint32_t x, const uint32_t y, const uint32_t m) {
    return static_cast<uint32_t>((static_cast<uint64_t>(x) * y) % m);
  }
  HashT operator+ (const HashT rhs) const {
    return HashT (addm (r1, rhs.r1, p1), addm (r2, rhs.r2, p2));
  }
  HashT operator- (const HashT rhs) const {
    return HashT (subm (r1, rhs.r1, p1), subm (r2, rhs.r2, p2));
  }
  HashT operator* (const HashT rhs) const {
    return HashT (mulm (r1, rhs.r1, p1), mulm (r2, rhs.r2, p2));
  }
  uint64_t get () const {
    return (static_cast<uint64_t> (r1) << 32) + r2;
  }
  HashT () : r1 (0), r2 (0) {}
  HashT (const uint32_t _r1, const uint32_t _r2) : r1 (_r1), r2 (_r2) {}
};

class PolyHash {
  private:
  const string::size_type n;
  vector<HashT> h, d;
  public:
  static pair<int, int> randomBase (mt19937 &rnd) {
    uniform_int_distribution<int> uid (256, 0x7fffffff);
    return { uid (rnd), uid (rnd) };
  }
  PolyHash (const string s, pair<int, int> base) : n (s.size ()), h (n + 1), d (n + 1) {
    const HashT p (base.first, base.second);
    h[0] = HashT (0, 0);
    d[0] = HashT (1, 1);
    for (unsigned i = 0; i < n; ++i) {
      uint32_t c = s[i];
      h[i+1] = h[i] * p + HashT (c, c);
      d[i+1] = d[i] * p;
    }
  }
  //[l, r)
  uint64_t operator() (const int l, const int r) const {
    return (h[r] - h[l] * d[r - l]).get ();
  }
};
