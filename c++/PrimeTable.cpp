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
  void getPrimes (vector<int> &r) {
    vector<int> b;
    int i;
    b.push_back (2);
    for (i = 3; i <= n; i += 2) {
      if (a[i>>6] & (1U << ((i >> 1) & 31))) {
        b.push_back (i);
      }
    }
    b.swap (r);
  }
  //generate all primes less than N (p[i] < N)
  PrimeTable (int maxN) : n (maxN)
  {
    const int nMaxSieveHalf = n / 2,
              nMaxSqrt = (int) floor (sqrt (n) / 2 + 1e-9);
    int i;
    a.resize ((n + 63) >> 6);
    fill (a.begin (), a.end (), -1);
    a[0] = -2;
    for (i = 1; i < nMaxSqrt; ++i) {
      if (a[i >> 5] & (1U << (i & 31))) {
        for (int j = 2 * i * (i+1); j < nMaxSieveHalf; j += i + i + 1) {
          a[j >> 5] &= ~(1U << (j & 31));
        }
      }
    }
  }
};

// x = prod_{i=0..k-1} r[i].firsr ^ r[i].second
void factor (const vector<int> &primes, int x, vector<pair<int, int> > &r) {
  vector<pair<int, int> > a;
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
  a.swap (r);
}

void factors_rec (int y, const vector<pair<int, int> > &c, size_t depth, vector<int> &o) {
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

void factors (const vector<pair<int, int> > &c, vector<int> &r) {
  r.clear ();
  factors_rec (1, c, 0, r);
  sort (r.begin (), r.end ());
}
