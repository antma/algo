import std.algorithm;
import std.array;
import std.conv;
import std.math;
import std.range;
import std.stdio;
import std.string;

int[] computePrefixFunction(T) (const T[] s) {
  immutable n = s.length;
  auto p = new int[n];
  p[0] = 0;
  int k = 0;
  foreach (q; 1 .. n) {
    while (k > 0 && s[k] != s[q]) {
      k = p[k-1];
    }
    if (s[k] == s[q]) {
      k++;
    }
    p[q] = k;
  }
  return p;
}

int[] kmpMatch(T,bool all=true) (const T[] t, const T[] p) {
  auto pi = computePrefixFunction (p);
  immutable m = p.length;
  size_t q;
  int[] off;
  foreach (i, c; t) {
    while (q > 0 && c != p[q]) {
      q = pi[q-1];
    }
    if (c == p[q]) {
      ++q;
    }
    if (q == m) {
      off ~= (i - (m - 1)).to!int;
      static if (all) {
        q = pi[q-1];
      } else {
        return off;
      }
    }
  }
  return off;
}

int[] computeZFunction(T) (const T[] s) {
  immutable n = s.length;
  auto z = new int[n];
  size_t l, r;
  foreach (i; 1 .. n) {
    debug stderr.writefln ("i = %d, l = %d, r = %d", i, l, r);
    int k;
    if (i <= r) {
      k = (min (r - i + 1, z[i - l]));
    }
    while (i + k < n && s[k] == s[i+k]) {
      ++k;
    }
    immutable j = i + k - 1;
    if (j > r) {
      l = i;
      r = j;
    }
    z[i] = k;
  }
  return z;
}

unittest {
  import std.stdio;
  writeln ("Testing string_functions.d ...");
  assert (equal (computePrefixFunction ("ababababca"), [0, 0, 1, 2, 3, 4, 5, 6, 0, 1]));
}
