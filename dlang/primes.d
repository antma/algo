import std.algorithm, std.conv, std.math, std.range;
/*
import std.algorithm;
import std.array;
import std.conv;
import std.math;
import std.stdio;
import std.string;
*/

import std.bitmanip;

class PrimeTable {
  private:
  BitArray a;
  size_t n;
  public:
  bool isPrime (size_t i) const pure nothrow @nogc {
    return (i & 1) ? !a[i>>>1] : (i == 2);
  }
  size_t [] primes () const pure nothrow {
    auto p = new size_t[0];
    if (n > 2) {
      p ~= 2;
      foreach (i; iota (3, n, 2)) {
        if (!a[i>>>1]) {
          p ~= i;
        }
      }
    }
    return p;
  }
  this (size_t n_) pure {
    n = n_;
    a.length = n;
    auto m = n >> 1;
    a[0] = true;
    foreach (i; 1 .. ceil((sqrt (n.to!(double)) - 1.0)).to!(size_t)) {
      if (!a[i]) {
        for (size_t j = 2 * i * (i + 1); j < m; j += (i << 1) + 1) {
          a[j] = true;
        }
      }
    }
  }
};

import std.stdio, std.string;

unittest {
  writeln ("Testing primes.d ...");
  auto primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199];
  foreach (n; 1 .. 200) {
    auto pt = new PrimeTable (n);
    auto p = primes.assumeSorted.lowerBound (n).map! (i => i.to!(size_t));
    auto q = pt.primes ();
    assert (equal (q, p), "n = %d, expected %s, result %s".format (n, p, q));
  }
}
