#include <vector>
//#include <algorithm>
#include <cstdint>

using namespace std;

template<typename T> class LinearSieveWithCnt {
  using F1 = T (*) (const int);
  using F2 = T (*) (const T, const int, const int);
  private:
  vector<bool> composite;
  vector<T> f;
  vector<int> primes;
  vector<uint8_t> cnt;
  public:
  T operator[] (size_t index) const {
    return f[index];
  }
  vector<T> move () {
    vector<T> r;
    r.swap (f);
    return r;
  }
  LinearSieveWithCnt (const int n, F1 prime, F2 op) : composite (n, false), f (n), cnt (n) {
    f[1] = 1;
    for (int i = 2; i < n; ++i) {
      if (!composite[i]) {
        primes.push_back (i);
        f[i] = prime (i);
        cnt[i] = 1;
      }
      for (const int j : primes) {
        const int k = i * j;
        if (k >= n) {
          break;
        }
        composite[k] = true;
        if (!(i % j)) {
          cnt[k] = cnt[i];
          f[k] = op (f[i], j, static_cast<int> (++cnt[k]));
          break;
        } else {
          f[k] = f[i] * f[j];
          cnt[k] = 1;
        }
      }
    }
  }
};

int dsPrime (const int p) { return 2; }
int dsDivides (const int q, const int j, const int k) {
  return (q / k) * (k + 1);
}

vector<int> divisorsSieve (int n) {
  LinearSieveWithCnt<int> s (n, &dsPrime, &dsDivides);
  return s.move ();
}

//////////////////// UNITTEST ////////////////////
#include <cassert>
#include <iostream>
class Unittest {
  public:
  Unittest() {
    cerr << "Testing " << __FILE__ << endl;
    auto v = divisorsSieve (10);
    vector<int> q = {1, 2, 2, 3, 2, 4, 2, 4, 3};
    assert (equal (q.cbegin (), q.cend (), ++v.cbegin ()));
  }
};
Unittest linear_sieve_unittest;
