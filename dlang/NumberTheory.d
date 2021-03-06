import core.bitop;
import std.algorithm;
import std.bigint;
import std.bitmanip;
import std.conv;
import std.exception;
import std.functional;
import std.math;
import std.numeric;
import std.random;
import std.range;
import std.traits;
import std.typecons;

pure nothrow @nogc
T gcdext(T) (T a, T b, ref T x, ref T y) {
  if (!b) {
    x = 1;
    y = 0;
    return a;
  }
  T res = gcdext (b, a % b, y, x);
  y -= x * (a / b);
  return res;
}

pure
X genericPower(alias mul, X, Y) (X x, Y y, X one) if (isUnsigned!Y) {
  if (!y) return one;
  X b = x;
  while (!(y & 1)) {
    b = binaryFun!mul (b, b);
    y >>>= 1;
  }
  X a = b;
  y >>>= 1;
  while (y) {
    b = binaryFun!mul (b, b);
    if (y & 1) {
      a = binaryFun!mul (a, b);
    }
    y >>>= 1;
  }
  return a;
}

final class PrimeTable {
  private:
  BitArray a;
  size_t n;
  public:
  pure nothrow @nogc
  bool isPrime (size_t i) const {
    return (i & 1) ? !a[i>>>1] : (i == 2);
  }
  pure
  int[] primes () const {
    int[] p;
    if (n > 2) {
      p ~= 2;
      p ~= (~a).bitsSet.map! (i => (2 * i + 1).to!int).array;
    }
    return p;
  }
  pure
  this (size_t _n) {
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

pure
Factorization factorizationTrialDivision (ulong x, in int[] primes) in {
  assert (primes.takeExactly (2).equal ([2, 3]));
} body {
  Factorization f;
  foreach (p; chain (primes, iota (primes.back + 2, int.max, 2))) {
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

pure nothrow
Factorization factorizationSieveArray (int x, in int[] sa) {
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

ulong squareMod (ulong a, ulong n) {
  asm {
    mov RAX, a;
    mov RBX, n;
    mul RAX, RAX;
    div RBX;
    mov RAX, RDX;
  }
}

Factorization factorizationPollardRho (ulong n, in int[] small_primes) {
  enum max_iterations = 10_000;
  ulong[] r;
  foreach (p; small_primes) {
    if (p * p > n) {
      break;
    }
    if (!(n % p)) {
      do {
        r ~= p;
        n /= p;
      } while (!(n % p));
    }
  }
  void factor (ulong n) {
    if (PrimalityTest64.isPrime (n)) {
      r ~= n;
      return;
    }
    while (true) {
      int k = 2;
      long x = uniform (0, n);
      long y = x;
      bool success;
      foreach (i; 2 .. max_iterations) {
        long xi = squareMod (x, n);
        if (xi > 0) {
          --xi;
        } else {
          xi = n - 1;
        }
        ulong d = gcd (y - xi, n);
        if (d != 1 && d != n) {
          factor (d);
          n /= d;
          success = true;
          break;
        }
        if (i == k) {
          y = xi;
          k = k << 1;
        }
        x = xi;
      }
      if (success && PrimalityTest64.isPrime (n)) {
        r ~= n;
        break;
      }
    }
  }
  if (n > 1) {
    factor (n);
  }
  sort (r);
  Factorization f;
  foreach (p; r.group ()) {
    f ~= tuple!("p", "c")(p[0], p[1]);
  }
  return f;
}

pure nothrow
auto divisorsFromFactorization(T) (in Factorization f, bool sorted = true)
  if (isIntegral!T) {
  T[] r;
  void go (size_t k, T cur) {
    if (k == f.length) {
      r ~= cur;
    } else {
      foreach (i; 0 .. f[k].c + 1) {
        go (k + 1, cur);
        cur *= f[k].p;
      }
    }
  }
  go (0, 1);
  if (sorted) {
    sort (r);
  }
  return r;
}

pure
immutable(int[]) sieveArray (int n) {
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
  return assumeUnique (a);
}

pure nothrow @nogc
byte mu (byte acc, int p, int c) {
  if (c > 1 || !acc) return 0;
  return (acc == 1) ? -1 : 1;
}

pure nothrow @nogc
int numberOfDivisors (int acc, int p, int c) {
  return acc * (c + 1);
}

pure nothrow @nogc
long sumOfDivisors (long acc, int p, int c) {
  return acc * ((p.to!long ^^ (c + 1) - 1) / (p - 1));
}

pure nothrow @nogc
int totient (int acc, int p, int c) {
  return acc * (p ^^ (c - 1)) * (p - 1);
}

pure
T[] sieveArrayDP(T) (in int[] sa, T function(T acc, int p, int c) pure nothrow @nogc op, T base) {
  T[] b = uninitializedArray! (T[]) (sa.length);
  b[0] = 0;
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

pure nothrow
int[][] divisorsArray (int n) {
  auto d = new int[][n];
  foreach (i; 2 .. n) {
    foreach (j; iota (i, n, i)) {
      d[j] ~= i;
    }
  }
  return d;
}

//////////////////// primality testing ////////////////////
final class PrimalityTest32 {
  pure
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
  pure
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

final class Montgomery64 {
  //R = 2 ^ 64
  //phi(R) = 2 ^ 63
  immutable ulong n, r_mod_n, neg_r_mod_n, r_div_n, r1, n1, rr;
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
  pure
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

final class PrimalityTest64 {
  private static bool witness (ulong a, const Montgomery64 m) {
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

class PrimeBlock {
  enum p = 30030;
  static immutable small_primes = [2,3,5,7,11,13];
  static immutable int[] idx, delta;
  static immutable ushort[] tbl;
  static int l;
  int[] w;
  ulong[] a;
  ulong off, end;
  immutable int m;
  static this () {
    auto b = new ushort[p];
    foreach (i; small_primes) for (int j = 0; j < p; j += i) b[j] = ushort.max;
    auto t = iota (0, p).filter! (i => !b[i]).array;
    idx = assumeUnique (t);
    l = idx.length.to!int;
    auto d = new int[l];
    foreach (i; 0 .. l - 1) d[i] = idx[i+1] - idx[i];
    d[l-1] = idx[0] + p - idx[l-1];
    delta = assumeUnique (d);
    foreach (i, k; idx) b[k] = i.to!ushort;
    foreach_reverse (i, k; b) if (k == ushort.max) b[i] = b[i+1];
    tbl = assumeUnique (b);
  }
  bool g(int u)(bool delegate(ulong) op) {
    foreach (i; 0 .. a.length) {
      auto x = ~a[i];
      while (x) {
        immutable j = (i << 6) + bsf (x);
        if (op (off + idx[j % l] + p * (j / l))) return true;
        x &= x - 1;
        static if (u) x &= ~a[i];
      }
    }
    return false;
  }
  void h (int q) {
    auto u = (off + q - 1) / q;
    int v = u % p, k = tbl[v];
    u += idx[k] - v;
    int o = (u * q - off).to!int;
    while (o < m) {
      int y = o / p, x = tbl[o - y * p] + y * l;
      a[x >> 6] |= 1UL << (x & 63);
      o += q * delta[k];
      if (++k == l) k = 0;
    }
  }
  this (int n) {
    n = ((n + p - 1) / p) * p;
    while (n & 7) n += p;
    m = n;
    a = new ulong[((m / p) * l) / 64];
    a[0] |= 1;
    off = 0;
    g!1 (delegate bool(ulong x) {
      if (x * x >= m) return true;
      w ~= x.to!int;
      h (x.to!int);
      return false;
    });
    g!0 (delegate bool(ulong x) {
      w ~= x.to!int;
      return false;
    });
    off = m;
  }
  void gen (bool delegate(ulong) op, ulong start) {
    bool s;
    if (!start) {
      foreach (p; small_primes) if (op (p)) return;
      foreach (p; w) if (op (p)) return;
      off = m;
    } else off = start;
    do {
      auto e = off + m;
      a[] = 0;
      foreach (p; w) {
        if (p.to!long * p >= e) break;
        h (p);
      }
      s = g!0 (op);
      off = e;
    } while(!s);
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

  assert (factorizationTrialDivision ((1L << 60) - 1, [2, 3]).equal(
    [ tuple (3, 2), tuple (5, 2), tuple (7, 1), tuple (11, 1),
      tuple (13, 1), tuple (31, 1), tuple (41, 1), tuple (61, 1),
      tuple (151, 1), tuple (331, 1), tuple (1321, 1) ]));

  assert (factorizationPollardRho (1, [2, 3, 5]).empty);
  assert (factorizationPollardRho (4, [2, 3, 5]).equal([tuple (2, 2)]));

  assert (factorizationPollardRho (571611561829541, [2, 3, 5]).equal(
    [ tuple (239, 2), tuple (10007, 1), tuple (1000003, 1)]));

  auto nd = sieveArrayDP (sa, &numberOfDivisors, 1);
  assert (nd[14] == 4);
  assert (nd[15] == 4);
  auto mf = sieveArrayDP (sa, &mu, 1);
  assert (mf[1 .. 13].equal([1, -1, -1, 0, -1, 1, -1, 0, 0, 1, -1, 0]));

  auto sd = sieveArrayDP (sa, &sumOfDivisors, 1L);
  assert (sd[220] == 284 + 220 && sd[284] == 220 + 284);
  auto pt = new PrimeTable (1_000_000);
  foreach (p; 1 .. 1_000_000) {
    assert (pt.isPrime (p) == PrimalityTest32.isPrime (p));
  }
  assert (PrimalityTest64.isPrime (999999999989));
  assert (divisorsArray(7).equal ([ [], [], [2], [3], [2, 4], [5], [2, 3, 6]]));

  auto sa_large = sieveArray (50_000);
  auto sd_large = sieveArrayDP (sa_large, &sumOfDivisors, 1L);
  assert (sd_large.all! (i => i >= 0));

  assert (equal (divisorsFromFactorization!int (factorizationTrialDivision (24, [2, 3]), true), [1, 2, 3, 4, 6, 8, 12, 24]));

}
