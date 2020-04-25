#include <algorithm>
#include <vector>
#include <cassert>
#include <numeric>

using namespace std;
enum class NumberOfSolutions { none = 0, unique = 1, infinity = 2};

vector<double> linsolve (vector<vector<double> >& a, vector<double>& b, const double eps, NumberOfSolutions& solutions) {
  const size_t m = a.size ();
  assert (m == b.size ());
  const size_t n = a[0].size ();
  vector<size_t> p (n);
  iota (p.begin (), p.end (), 0);
  size_t i = 0, j = 0;
  while (true) {
    double v = 0.0;
    size_t bi = n, bj = m;
    for (auto x = i; x < n; ++x) {
      const auto k = p[x];
      for (auto l = j; l < m; ++l) {
        const double w = abs (a[l][k]);
        if (v < w) {
          v = w;
          bi = x;
          bj = l;
        }
      }
    }
    if (bi == n) break;
    if (i != bi) {
      swap (p[i], p[bi]);
    }
    if (j != bj) {
      swap (a[j], a[bj]);
      swap (b[j], b[bj]);
    }
    const auto& c = a[j];
    v = 1.0 / c[p[i]];
    for (auto l = j + 1; l < m; ++l) {
      const double w = v * a[l][p[i]];
      auto& d = a[l];
      for (auto k = i; k < n; ++k) {
        d[p[k]] -= c[p[k]] * w;
      }
      b[l] -= b[j] * w;
    }
    ++i; ++j;
  }
  for (auto l = j; l < m; ++l) if (abs (b[l]) > eps) {
    solutions = NumberOfSolutions::none;
    return {};
  }
  if (j < n) {
    solutions = NumberOfSolutions::infinity;
    return {};
  }
  vector<double> x (n);
  for (j = n - 1; j < n; --j) {
    double w = b[j];
    for (i = j; i < n; ++i) {
      w -= x[p[i]] * a[j][p[i]];
    }
    x[p[j]] = w / a[j][p[j]];
  }
  solutions = NumberOfSolutions::unique;
  return x;
}
