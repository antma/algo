#include <algorithm>
#include <vector>
#include <cmath>

using namespace std;

class PrimeTable {
  private:
  int n;
  vector<unsigned int> a;
  public:
  bool isPrime (int i) const {
    if (!(i & 1)) return i == 2;
    return (a[i>>6] & (1U << ((i >> 1) & 31))) != 0;
  }
  vector<int> getPrimes () const {
    vector<int> b;
    if (n <= 2) return b;
    b.push_back (2);
    for (int i = 3; i < n; i += 2) {
      if (a[i>>6] & (1U << ((i >> 1) & 31))) {
        b.push_back (i);
      }
    }
    return b;
  }
  //generate all primes less than N (p[i] < N)
  PrimeTable (int maxN) : n (maxN), a ( (n + 63) >> 6, -1) {
    const int m = n / 2;
    --a[0];
    for (int i = 1; i * i < n; ++i) {
      if (a[i >> 5] & (1U << (i & 31))) {
        for (int j = 2 * i * (i + 1); j < m; j += i + i + 1) {
          a[j >> 5] &= ~(1U << (j & 31));
        }
      }
    }
  }
};

using Factorization = vector<pair<int, int> >;
// x = prod_{i=0..k-1} r[i].first ^ r[i].second
Factorization factorization (const vector<int> &primes, int x) {
  Factorization a;
  for (const auto j : primes) {
    if (j * j > x) break;
    if (!(x % j)) {
      int c = 0;
      do {
        ++c;
        x /= j;
      } while (!(x % j));
      a.emplace_back (j, c);
    }
  }
  if (x > 1) {
    a.emplace_back (x, 1);
  }
  return a;
}

void factors_rec (int y, const Factorization &c, size_t depth, vector<int> &o) {
  if (depth == c.size ()) {
    o.push_back (y);
    return;
  }
  const int n = c[depth].second;
  for (int k = 0; ; ++k) {
    factors_rec (y, c, depth + 1, o);
    if (k == n) break;
    y *= c[depth].first;
  }
}

vector<int> factors (const Factorization &c) {
  vector<int> r;
  factors_rec (1, c, 0, r);
  sort (r.begin (), r.end ());
  return r;
}

//////////////////// UNITTEST ////////////////////
#include <cstdint>
#include <cassert>
#include <iostream>
class PrimeTableUnittest {
  public:
  PrimeTableUnittest() {
    cerr << "Testing " << __FILE__ << endl;
    PrimeTable p (1000000);
    uint64_t s = 0;
    vector<int> a = p.getPrimes ();
    for (const auto i : a) s += i;
    assert (s == UINT64_C(37550402023));
    vector<int> primes = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199};
    for (int n = 1; n < 200; ++n) {
      auto r = equal_range (primes.cbegin (), primes.cend (), n);
      assert ( (r.first == r.second) == !p.isPrime (n));
    }
    for (int n = 1; n < 200; ++n) {
      PrimeTable pt (n);
      auto it = lower_bound (primes.cbegin (), primes.cend (), n);
      auto q = pt.getPrimes ();
      assert (q.size () == static_cast<size_t> (it - primes.cbegin ()));
      assert (equal (q.cbegin (), q.cend (), primes.cbegin ()));
    }
  }
};

PrimeTableUnittest primetable_unittest;

