//Origin: https://codeforces.com/blog/entry/17507
#include <vector>
#include <algorithm>
#include <random>
#include <cstdint>
#include <ctime>

using namespace std;

class PolyHash {
  private:
  int n;
  vector<uint64_t> h;
  vector<uint64_t> d;
  public:
  static uint32_t randomP () {
    static int primes[20] = { 2147483647, 2147483629, 2147483587, 2147483579, 2147483563, 2147483549, 2147483543, 2147483497, 2147483489, 2147483477, 2147483423, 2147483399, 2147483353, 2147483323, 2147483269, 2147483249, 2147483237, 2147483179, 2147483171, 2147483137};
    mt19937 gen (time (0));
    return primes[uniform_int_distribution<int> (0, 19) (gen)];
  }
  PolyHash (const string &s, uint32_t p) :
    n (s.length ()),
    h (n + 1),
    d (n + 1)
  {
    h[0] = 0;
    d[0] = 1;
    for (int i = 0; i < n; ++i) {
      h[i+1] = h[i] * p + s[i];
      d[i+1] = d[i] * p;
    }
  }
  //[l, r)
  uint64_t get (int l, int r) {
    return h[r] - h[l] * d[r - l];
  }
};
