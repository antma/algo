import std.algorithm;
import std.conv;
import std.math;
import std.range;
import std.bitmanip;
import std.typecons;

class PrimeTable {
  private:
  BitArray a;
  size_t n;
  public:
  bool isPrime (size_t i) const pure nothrow @nogc {
    return (i & 1) ? !a[i>>>1] : (i == 2);
  }
  int[] primes () const pure {
    int[] p;
    if (n > 2) {
      p ~= 2;
      p ~= (~a).bitsSet.map! (i => (2 * i + 1).to!int).array;
    }
    return p;
  }
  this (size_t _n) pure {
    n = _n;
    auto m = n >> 1;
    a.length = max (1, m);
    a[0] = true;
    foreach (i; 1 .. ceil((sqrt (n.to!(double)) - 1.0)).to!(size_t)) {
      if (!a[i]) {
        immutable k = (i << 1) + 1;
        for (size_t j = 2 * i * (i + 1); j < m; j += k) {
          a[j] = true;
        }
      }
    }
  }
}

alias Factorization = Tuple!(ulong, "p", uint, "c")[];
Factorization factorizationTrialDivision (ulong x, int[] primes) {
  Factorization f;
  foreach (p; primes) {
    if (p.to!ulong * p > x) {
      break;
    }
    uint c;
    while (!(x % p)) {
      x /= p;
      ++c;
    }
    if (c) {
      f ~= tuple!("p", "c")(p.to!ulong, c);
    }
  }
  if (x > 1) {
    f ~= tuple!("p", "c")(x, 1U);
  }
  return f;
}

int[] sieveArray (int n) {
  auto a = chain (only (0), iota (1, n + 1).map! (i => (i & 1) ? i : 2)).array;
  foreach (p; iota (3, (sqrt(n.to!double) + 1e-9).to!int + 1, 2)) {
    if (a[p] == p) {
      foreach (o; iota (p * p, n + 1, 2 * p)) {
        if (a[o] == o) {
          a[o] = p;
        }
      }
    }
  }
  return a;
}

unittest {
  import std.stdio, std.string;
  writeln ("Testing ", __FILE__, " ...");
  auto primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199];
  foreach (n; 1 .. 200) {
    auto pt = new PrimeTable (n);
    auto p = primes.assumeSorted.lowerBound (n).map! (i => i.to!(size_t));
    auto q = pt.primes ();
    assert (equal (q, p), "n = %d, expected %s, result %s".format (n, p, q));
  }
  assert (equal ([tuple (17UL, 1U)], factorizationTrialDivision (17, primes)));
  assert (equal (sieveArray (9), [0, 1, 2, 3, 2, 5, 2, 7, 2, 3]));
}
