import std.algorithm;
import std.array;
import std.conv;
import std.format;
import std.range;

class SuffixArray {
  string s;
  int [] O;
  int n;

  final void counting_sort (int m, int [] c, int [] p, int [] o) const {
    auto cnt = new int[m];
    foreach (i; 0 .. p.length) {
      ++cnt[c[p[i]]];
    }
    foreach (i; 1 .. cnt.length) {
      cnt[i] += cnt[i-1];
    }
    for (int i = p.length.to!(int) - 1; i >= 0; --i) {
      o[--cnt[c[p[i]]]] = p[i];
    }
  }

  this (string s_) {
    s = s_ ~ 0.to!(char);
    n = s.length.to!(int);
    auto c = s.map! (to!(int)).array;
    immutable a = 1 + c.reduce! (max);
    auto p = new int[n];
    counting_sort (a, c, iota (0, n).array, p);
    c[p[0]] = 0;
    int m = 1;
    foreach (i; 1 .. n) {
      if (s[p[i]] != s[p[i-1]]) {
        ++m;
      }
      c[p[i]] = m - 1;
    }
    auto T = new int[n];
    for (int step = 1; step < n; step <<= 1) {
      foreach (i; 0 .. n) {
        p[i] -= step;
        if (p[i] < 0) {
          p[i] += n;
        }
      }
      counting_sort (m, c, p, T);
      auto t = p; p = T; T = t;
      T[p[0]] = 0;
      m = 1;
      foreach (i; 1 .. n) {
        if (c[p[i]] != c[p[i-1]] || c[(p[i] + step) % n] != c[(p[i-1] + step) % n]) {
          ++m;
        }
        T[p[i]] = m - 1;
      }
      t = c; c = T; T = t;
    }
    assert (p[0] == n - 1);
    O = p[1 .. n];
    --n;
  }
};

class LCPSuffixArray : SuffixArray {
  int [] R; //reverse permutation
  int [] LCP;
  final int lcp (int l, int r) {
    return (r - l == 1) ? LCP[r] : LCP[n + 1 + ((l + r) >> 1)];
  }
  final int lcp_build (int l, int r) {
    if (r - l == 1) {
      return LCP[r];
    }
    immutable m = (l + r) >> 1;
    LCP[n + 1 + m] = min (lcp_build (l, m), lcp_build (m, r));
    return LCP[n + 1 + m];
  }
  this (string s) {
    super (s);
    LCP = new int [2 * n + 1];
    R = new int[n];
    foreach (i; 0 .. n) {
      R[O[i]] = i;
    }
    int l = 0;
    foreach (j; 0 .. n) {
      l = max (0, l - 1);
      immutable i = R[j];
      if (i > 0) {
        immutable k = O[i - 1];
        while ((j + l < n) && (k + l < n) && s[j + l] == s[k + l]) {
          ++l;
        }
      } else {
        l = 0;
      }
      LCP[i] = l;
    }
    LCP[n] = 0;
    lcp_build (-1, n);
  }
};

void check (string s) {
  string z = s ~ '$';
  auto sa = new SuffixArray (z);
  foreach (k; 1 .. s.length) {
    int i = sa.O[k-1];
    int j = sa.O[k];
    assert (s[i .. $] <= s[j .. $]);
  }
}

import std.random;
import std.stdio;

unittest {
  writeln ("Testing suffix_array.d ...");
  check ("abacabadaba");
  string t = iota(0, 1000).map! (x => (uniform(0, 26) + 97).to!(char)).text;
  check (t);
}
