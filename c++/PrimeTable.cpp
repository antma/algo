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
    const int nMaxSieveHalf = n / 2,
              nMaxSqrt = (int) floor (sqrt (n) / 2 + 1e-9);
    --a[0];
    for (int i = 1; i < nMaxSqrt; ++i) {
      if (a[i >> 5] & (1U << (i & 31))) {
        for (int j = 2 * i * (i + 1); j < nMaxSieveHalf; j += i + i + 1) {
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
