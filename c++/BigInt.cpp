#include <vector>
#include <algorithm>
#include <iostream>

using namespace std;

class BigInt {
  private:
  vector<short> v;
  void incrementFrom (int k, int c) {
    const int n = v.size ();
    for (; k < n && c; ++k) {
      v[k] += c;
      if (v[k] >= 10000) {
        v[k] = 0;
        c = 1;
      } else {
        c = 0;
      }
    }
    if (c) {
      v.push_back (1);
    }
  }
  void decrementFrom (int k, int c) {
    const int n = v.size ();
    for (; k < n && c; ++k) {
      v[k] -= c;
      if (v[k] < 0) {
        v[k] += 10000;
        c = 1;
      } else {
        c = 0;
      }
    }
    while (!v.empty () && !v.back ()) v.pop_back ();
  }
  public:
  /* 0 <= n < 10000 */
  BigInt (short n = 0) : v (1, n) { }
  BigInt (const BigInt &l) : v (l.v) { }
  /*0 <= a < 10000 */
  BigInt (const string &s) {
    //int l = s.length ();
    int i = s.length ();
    while (i > 0) {
      int j = i - 4;
      if (j < 0) j = 0;
      int d = 0;
      if (i > 3) d += (s[i-4] - '0') * 1000;
      if (i > 2) d += (s[i-3] - '0') * 100;
      if (i > 1) d += (s[i-2] - '0') * 10;
      d += s[i-1] - '0';
      i = j;
      v.push_back (d);
    }
    if (v.empty ()) v.push_back (0);
  }
  BigInt& operator++() {
    incrementFrom (0, 1);
    return *this;
  }
  BigInt& operator+= (const short rhs) {
    incrementFrom (0, rhs);
    return *this;
  }
  BigInt& operator-= (const short rhs) {
    decrementFrom (0, rhs);
    return *this;
  }
  BigInt& operator+= (const BigInt &rhs) {
    int k, c = 0;
    for (k = 0; k < (int) rhs.v.size (); ++k) {
      c += rhs.v[k];
      if (k < (int) v.size ()) {
        c += v[k];
      }
      int x = c;
      if (c >= 10000) {
        x -= 10000;
        c = 1;
      } else {
        c = 0;
      }
      if (k < (int) v.size ()) v[k] = x;
      else v.push_back (x);
    }
    if (c > 0) {
      incrementFrom (k, 1);
    }
    return *this;
  }
  BigInt& operator*= (short a) {
    int c = 0, n = v.size ();
    for (int i = 0; i < n; ++i) {
      c += ((int) a) * ((int) v[i]);
      v[i] = c % 10000;
      c /= 10000;
    }
    if (c) {
      v.push_back ((short) c);
    }
    return *this;
  }
  BigInt& operator/= (short a) {
    int d = 0, k;
    for (k = v.size() - 1; k >= 0; --k) {
      d *= 10000;
      d += v[k];
      v[k] = d / a;
      d -= v[k] * a;
    }
    k = v.size ();
    for (;;) {
      --k;
      if (!k) break;
      if (!v[k]) {
        v.pop_back ();
      }
    }
    return *this;
  }
  void print (ostream &o) const {
    const int n = v.size();
    o << v[n-1];
    for(int i = n- 2;i >= 0; --i) {
      o.width (4);
      o.fill ('0');
      o << v[i];
    }
  }
};

ostream& operator<< (ostream &o, const BigInt &l) {
  l.print (o);
  return o;
}
