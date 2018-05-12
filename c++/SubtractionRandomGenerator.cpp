#include <cassert>

class SubtractionRandomGenerator {
private:
  static const int M    = 0x7fffffff;
  static const int SEED = 0x5EED5EED >> 3;
  int a[56],u,v;
  double fact1,fact2;
public:
  SubtractionRandomGenerator (int seed) {
    int i, j, k;
    seed = SEED - seed;
    if (seed < 0) {
      seed = -seed;
    }
    a[55] = j = seed % M;
    k = 1;
    for (i = 1; i <= 54; ++i) {
      int idx = (21 * i) % 55;
      a[idx] = k;
      k = j - k;
      if (k < 0) {
        k += M;
      }
      j=a[idx];
    }
    for (k = 1; k <= 4; k++) for (i = 1; i <= 55; ++i) {
      a[i] -= a[1+(i+30) % 55];
      if (a[i] < 0) a[i] += M;
    }
    u = 0;
    v = 31;
    fact1 = 1.0 / M;
    fact2 = 1.0 / (M-1);
  }
  int rand() {
    int j;
    if (++u == 56) u = 1;
    if (++v == 56) v = 1;
    j = a[u]-a[v];
    if (j < 0) j += M;
    return a[u]=j;
  }
  inline double operator()() { return rand() * fact2; }
  inline int operator() (int N) {
    assert (N > 0);
    return (int) (rand() * fact1 * N);
  }
};
