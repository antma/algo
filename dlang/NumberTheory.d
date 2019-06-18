import core.bitop;
import std.algorithm;
import std.bigint;
import std.bitmanip;
import std.conv;
import std.functional;
import std.math;
import std.numeric;
import std.random;
import std.range;
import std.traits;
import std.typecons;

T gcdext(T) (T a, T b, ref T x, ref T y) pure nothrow @nogc {
  if (!b) {
    x = 1;
    y = 0;
    return a;
  }
  T res = gcdext (b, a % b, y, x);
  y -= x * (a / b);
  return res;
}

X genericPower(alias mul, X, Y) (X x, Y y, X one) if (isUnsigned!Y) {
  X a = one, b = x;
  while (y > 0) {
    if (y & 1) {
      a = binaryFun!mul (a, b);
    }
    b = binaryFun!mul (b, b);
    y >>>= 1;
  }
  return a;
}

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

Factorization factorizationSieveArray (int x, int[] sa) {
  Factorization f;
  uint last, c;
  while (x > 1) {
    int p = sa[x];
    if (last != p) {
      if (last) {
        f ~= tuple! ("p", "c") (last.to!ulong, c);
      }
      c = 1;
      last = p;
    } else {
      ++c;
    }
    x /= p;
  }
  if (last) {
    f ~= tuple! ("p", "c") (last.to!ulong, c);
  }
  return f;
}

int[] sieveArray (int n) {
  auto a = chain (only (0), iota (1, n).map! (i => (i & 1) ? i : 2)).array;
  foreach (p; iota (3, sqrt(n.to!double).to!int + 1, 2)) {
    if (a[p] == p) {
      foreach (o; iota (p * p, n, 2 * p)) {
        if (a[o] == o) {
          a[o] = p;
        }
      }
    }
  }
  return a;
}

long sumOfDivisors (long acc, int p, int c) {
  return acc * ((p ^^ (c + 1) - 1) / (p - 1));
}

int totient (int acc, int p, int c) {
  return acc * (p ^^ (c - 1)) * (p - 1);
}

T[] sieveArrayDP(T) (int[] sa, T function(T acc, int p, int c) op, T base) {
  T[] b = uninitializedArray! (T[]) (sa.length);
  b[1] = base;
  foreach (i; 2 .. sa.length) {
    immutable p = sa[i];
    int c = 1, x = i.to!int / p;
    while (sa[x] == p) {
      ++c;
      x /= p;
    }
    b[i] = op (b[x], p, c);
  }
  return b;
}

//////////////////// primality testing ////////////////////
class PrimalityTest32 {
  private static bool witness (uint a, uint n) {
    immutable n1 = n - 1;
    immutable m = bsf (n1);
    uint x = genericPower!( (a, b) => ((a.to!ulong * b) % n).to!uint, uint, uint) (a, n1 >>> m, 1U);
    foreach (i; 0 .. m) {
      uint y = (x.to!ulong * x) % n;
      if (y == 1 && x != 1 && x != n1) {
         return true;
      }
      x = y;
    }
    return x != 1;
  }
  public static bool isPrime (uint n) {
    if (n <= 23) return ((1 << n) & 0x8a28ac) != 0;
    if (gcd (n, 223092870) > 1) return false;
    if (n <= 529) return true;
    if (witness (2, n) || witness (61, n)) return false;
    if (n < 916327) return true;
    if (witness (7, n)) return false;
    if (n < 4759123141)  return true;
    if (witness (3, n) || witness (24251, n)) return false;
    return true;
  }
}

class Montgomery64 {
  //R = 2 ^ 64
  //phi(R) = 2 ^ 63
  immutable ulong n, r_mod_n, neg_r_mod_n, r_div_n, r1, n1, rr;
  ulong reduce (BigInt t) const {
    ulong a = (t & ulong.max).to!ulong;
    a *= n1;
    t += BigInt (a) * n;
    t >>= 64;
    a = t.to!ulong;
    return (a >= n) ? a - n : a;
  }
  ulong mul_asm (ulong a, ulong b) const {
    asm {
      mov RAX, a;
      mul RAX, b;
      //RDX:RAD = (a * b)
      mov RCX, RDX;
      mov RBX, RAX;
      //t = RCX:RBX
      mul RAX, [RDI + n1.offsetof];
      mul RAX, [RDI + n.offsetof];
      add RAX, RBX;
      adc RCX, RDX;
      mov RAX, RCX;
    }
  }
  ulong mul (ulong a, ulong b) const {
    immutable r = mul_asm (a, b);
    return (r >= n) ? r - n : r;
  }
  ulong pow (ulong x, ulong y) const {
    ulong a = r_mod_n, b = x;
    while (y > 0) {
      if (y & 1) {
        a = mul (a, b);
      }
      b = mul (b, b);
      y >>>= 1;
    }
    return a;
  }
  this (ulong _n) {
    n = _n;
    ulong d = ulong.max / n;
    ulong m = ulong.max - d * n;
    if (++m == n) { m = 0; ++d; }
    r_mod_n = m;
    neg_r_mod_n = m ? n - m : 0;
    r_div_n = d;
    ulong x, y;
    gcdext (n, r_mod_n, y, x);
    y -= x * r_div_n;
    n1 = -y;
    r1 = x;
    rr = ((BigInt (m) * m) % n).to!ulong;
  }
}

class PrimalityTest64 {
  private static bool witness (ulong a, const ref Montgomery64 m) {
    immutable n1 = m.n - 1;
    immutable l = bsf (n1);
    a = m.mul (a, m.rr);
    ulong x = m.pow (a, n1 >>> l);
    foreach (i; 0 .. l) {
      ulong y = m.mul (x, x);
      if (y == m.r_mod_n && x != m.r_mod_n && x != m.neg_r_mod_n) {
         return true;
      }
      x = y;
    }
    return x != m.r_mod_n;
  }
  public static bool isPrime (ulong n, int tries = 5) {
    if (n <= uint.max) return PrimalityTest32.isPrime (n.to!uint);
    if (gcd (n, 223092870) > 1) return false;
    auto m = new Montgomery64 (n);
    foreach (t; 0 .. tries) {
      if (witness (uniform (2L, n - 1), m)) return false;
    }
    return true;
  }

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
  immutable int[] sa16 = [0, 1, 2, 3, 2, 5, 2, 7, 2, 3, 2, 11, 2, 13, 2, 3];
  foreach (n; 1 .. 16) {
    assert (equal (sieveArray (n), sa16[0 .. n]));
  }
  enum m = 300;
  auto sa = sieveArray (m);
  foreach (i; 1 .. m) {
    auto f1 = factorizationTrialDivision (i, primes), f2 = factorizationSieveArray (i, sa);
    assert (equal (f1, f2), format ("%s %s", f1, f2));
  }
  auto sd = sieveArrayDP (sa, &sumOfDivisors, 1L);
  assert (sd[220] == 284 + 220 && sd[284] == 220 + 284);
  auto pt = new PrimeTable (1_000_000);
  foreach (p; 1 .. 1_000_000) {
    assert (pt.isPrime (p) == PrimalityTest32.isPrime (p));
  }
}
