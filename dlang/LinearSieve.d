import std.algorithm;
import std.bitmanip;
import std.conv;
import std.traits;

final class LinearSieve(T)
  if (isIntegral!T) {
  private:
  BitArray composite;
  T[] f;
  int[] primes;
  public:
  T opIndex (size_t index) const {
    return f[index];
  }
  auto opSlice(size_t begin, size_t end) const {
    return f[begin .. end];
  }
  this (const int n, T function (const int) prime, T function (const T, const int) divides) {
    composite.length = n;
    f = new T[n];
    f[1] = 1;
    foreach (i; 2 .. n) {
      if (!composite[i]) {
        primes ~= i;
        f[i] = prime (i);
      }
      foreach (j; primes) {
        immutable k = i * j;
        if (k >= n) {
          break;
        }
        composite[k] = true;
        if (!(i % j)) {
          f[k] = divides(f[i], j);
          break;
        } else {
          f[k] = f[i] * f[j];
        }
      }
    }
  }
}

final class LinearSieveWithCnt(T)
  if (isIntegral!T) {
  private:
  BitArray composite;
  T[] f;
  int[] cnt;
  int[] primes;
  public:
  T opIndex (size_t index) const {
    return f[index];
  }
  auto opSlice(size_t begin, size_t end) const {
    return f[begin .. end];
  }
  this (const int n, T function (const int ) prime, T function (const T, const int, const int) op) {
    composite.length = n;
    f = new T[n];
    cnt = new int[n];
    f[1] = 1;
    foreach (i; 2 .. n) {
      if (!composite[i]) {
        primes ~= i;
        f[i] = prime (i);
        cnt[i] = 1;
      }
      foreach (j; primes) {
        immutable k = i * j;
        if (k >= n) {
          break;
        }
        composite[k] = true;
        if (!(i % j)) {
          f[k] = op (f[i], j, cnt[k] = cnt[i] + 1);
          //f[k] = (f[i] / (op (j, cnt[i])) * op (j, ++cnt[i]);
          break;
        } else {
          f[k] = f[i] * f[j];
          cnt[k] = 1;
        }
      }
    }
  }
}

auto sigma2Sieve (const int n) {
  return new LinearSieveWithCnt!long (n, function long (const int p) { return p.to!long * p + 1; }, function long (const long acc, const int p, const int c) {
    immutable long q = p.to!long * p, t = q ^^ c, old = (t - 1) / (q - 1);
    return (acc / old) * (old + t);
  });
}

auto mobiusSieve (const int n) {
  return new LinearSieve!int (n, function int (const int p) { return -1; }, function int (const int acc, const int j) {
    return 0;
  });
}

auto totientSieve (const int n) {
  return new LinearSieve!int (n, function int (const int p) { return p - 1; }, function int (const int acc, const int j) {
    return acc * j;
  });
}

final class HarmonicSeries {
  private:
  immutable ulong n;
  long cur;
  public:
  @property
  bool empty () const {
    return front < 0;
  }
  long front;
  void popFront () {
    if (front > n) {
      front = -1;
      return;
    }
    front = 1 + (n / cur);
    if (front > n) {
      front = n + 1;
    }
    cur = n / front;
  }
  this (ulong n) {
    this.n = cur = n;
    front = 1;
  }
}

unittest {
  import std.stdio, std.string, std.array;
  writeln ("Testing ", __FILE__, " ...");
  immutable mu = [1, -1, -1, 0, -1, 1, -1, 0, 0, 1, -1, 0];
  auto m = mobiusSieve (mu.length.to!int + 1);
  assert (m[1 .. mu.length + 1].equal (mu));
  immutable totients = [1, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12, 6, 8, 8, 16, 6, 18, 8, 12, 10, 22, 8, 20, 12, 18, 12, 28, 8, 30, 16, 20, 16, 24, 12, 36, 18, 24, 16, 40, 12, 42, 20, 24, 22, 46, 16, 42, 20, 32, 24, 52, 18, 40, 24, 36, 28];
  auto t = totientSieve (totients.length.to!int + 1);
  assert (t[1 .. totients.length + 1].equal (totients));
  immutable sigma2 = [
  	1, 5, 10, 21, 26, 50, 50, 85, 91, 130, 122, 210, 170, 250, 260, 341, 290, 455, 362, 546, 500, 610, 530, 850, 651, 850, 820, 1050, 842, 1300, 962, 1365, 1220, 1450, 1300, 1911, 1370, 1810, 1700, 2210, 1682, 2500, 1850, 2562, 2366, 2650, 2210, 3410, 2451, 3255];
  auto ts2 = sigma2Sieve (sigma2.length.to!int + 1);
  assert (ts2[1 .. sigma2.length + 1].equal (sigma2));
  assert (new HarmonicSeries (10).array.equal ([1, 2, 3, 4, 6, 11]));
}
