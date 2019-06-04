import std.algorithm;
import std.array;
import std.conv;
import std.format;
import std.range;
import std.typecons;

class SuffixArray {
  string s;
  int [] O;
  int n;

  final void countingSort (int m, int [] c, int [] p, int [] o) const pure nothrow {
    auto cnt = new int[m];
    foreach (i; 0 .. p.length) {
      ++cnt[c[p[i]]];
    }
    foreach (i; 1 .. cnt.length) {
      cnt[i] += cnt[i-1];
    }
    foreach (i; iota (0, p.length).retro) {
      o[--cnt[c[p[i]]]] = p[i];
    }
  }

  this (string s_) pure {
    s = s_ ~ 0.to!(char);
    n = s.length.to!(int);
    auto c = s.map! (to!(int)).array;
    immutable a = 1 + c.reduce! (max);
    auto p = new int[n];
    countingSort (a, c, iota (0, n).array, p);
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
      countingSort (m, c, p, T);
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
  final int lcp (int l, int r) const pure nothrow @nogc {
    return (r - l == 1) ? LCP[r] : LCP[n + 1 + ((l + r) >> 1)];
  }
  final int lcp_build (int l, int r) pure nothrow @nogc {
    if (r - l == 1) {
      return LCP[r];
    }
    immutable m = (l + r) >> 1;
    LCP[n + 1 + m] = min (lcp_build (l, m), lcp_build (m, r));
    return LCP[n + 1 + m];
  }
  final Tuple!(int, int) interval (in string x) {
    immutable lx = x.length;
    Tuple!(int, int) f (int u, int lu, int v, int lv) {
      if (u + 1 >= v) return tuple (int.min, int.min);
      int m = (u + v) >> 1;
      if (lu <= lcp (m, v) && lcp (m, v) < lv) {
        return f (m, lcp (m, v), v, lv);
      } else if (lu <= lv && lv < lcp (m, v)) {
        return f (u, lu, m, lv);
      } else if (lv <= lcp (u, m) && lcp (u, m) < lu) {
        return f (u, lu, m, lcp (u, m));
      } else if (lv <= lu && lu < lcp (u, m)) {
        return f (m, lu, v, lv);
      } else {
        int l = max (lu, lv);
        immutable off = O[m];
        immutable lm = n - off;
        immutable ml = min (lx, lm);
        while (l < ml && x[l] == s[off+l]) ++l;
        if (l == lx) {
          int e = m;
          while (u + 1 < e) {
            int j = (u + e) >> 1;
            if (lcp (j, e) < lx) {
              u = j;
            } else {
              e = j;
            }
          }
          if (lcp (u, e) >= lx) u = max (u - 1, -1);
          e = m;
          while (e + 1 < v) {
            int j = (e + v) >> 1;
            if (lcp (e, j) < lx) {
              v = j;
            } else {
              e = j;
            }
          }
          if (lcp (e, v) >= lx) v = min (v + 1, n);
          return tuple (u, v);
        } else if (l == lm || (l != lx && s[off+l] < x[l])) {
          return f (m, l, v, lv);
        } else {
          return f (u, lu, m, l);
        }
      }
    }
    return f (-1, 0, n, 0);
  }
  this (string s) pure {
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

import std.random;
import std.stdio;

unittest {
  writeln ("Testing ", __FILE__, " ...");
  void check (string s) {
    string z = s ~ '$';
    auto sa = new SuffixArray (z);
    foreach (k; 1 .. s.length) {
      int i = sa.O[k-1];
      int j = sa.O[k];
      assert (s[i .. $] <= s[j .. $]);
    }
  }
  check ("abacabadaba");
  string t = iota(0, 1000).map! (x => (uniform(0, 26) + 97).to!(char)).text;
  check (t);
}
