import std.algorithm;
import std.array;
import std.conv;
import std.format;
import std.range;

struct SuffixArray {
  string s;
  int [] O;
  
  void counting_sort (int m, int [] c, int [] p, int [] o) const {
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
    s = s_;
    int n = s.length.to!(int);
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
    O = p;
  }
};

void check (string s) {
  string z = s ~ '$';
  auto sa = SuffixArray (z);
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
