import std.algorithm, std.conv, std.stdio;

T gcd(T) (T a, T b) {
  if (a < b) {
    swap (a, b);
  }
  while (b) {
    T c = a % b; a = b; b = c;
  }
  return a;
}

T gcdext(T) (T a, T b, ref T x, ref T y) {
  if (b == 0) {
    x = 1;
    y = 0;
    return a;
  }
  T res = gcdext (b, a % b, y, x);
  y -= x * (a / b);
  return res;
}

struct IntM {
  static immutable q = 1_000_000_007;
  int v;
  this (int m) {
    v = m % q;
    if (v < 0) {
      v += q;
    }
  }
  IntM opAssign (int m) {
    if (v < 0) {
      v += q;
    }
    v = m % q;
    return this;
  }
  IntM opUnary (string op)() const if (op == "-") {
    return IntM ((q - v) % q);
  }
  ref IntM opUnary (string op)() if (op == "++") {
    if (++v >= q) {
      v -= q;
    }
    return this;
  }
  ref IntM opUnary (string op)() if (op == "--") {
    if (--v < 0) {
      v += q;
    }
    return this;
  }
  ref IntM opOpAssign (string op)(in IntM rhs) if (op == "+") {
    v += rhs.v;
    v %= q;
    return this;
  }
  ref IntM opOpAssign (string op)(in IntM rhs) if (op == "-") {
    v -= rhs.v;
    v %= q;
    return this;
  }
  ref IntM opOpAssign (string op)(in IntM rhs) if (op == "*") {
    v = ((v.to!(long)) * rhs.v.to!(long)) % q;
    return this;
  }
  IntM opBinary (string op)(in IntM rhs) const if (op == "+") {
    return IntM ( (v + rhs.v) % q);
  }
  IntM opBinary (string op)(in IntM rhs) const if (op == "-") {
    return IntM ( (v - rhs.v) % q);
  }
  IntM opBinary (string op)(in IntM rhs) const if (op == "*") {
    return IntM (((v.to!(long)) * rhs.v.to!(long)) % q);
  }
  IntM opBinary (string op)(in int rhs) const if (op == "^^") {
    IntM a = 1, b = this;
    int p = rhs;
    while (p > 0) {
      //a * (b ^ p) == x ^ rhs
      if (p & 1) {
        a *= b;
      }
      b *= b;
      p >>>= 1;
    }
    return a;
  }
  IntM opBinary (string op)(in int v) const if (op == "+" || op == "-" || op == "*") {
    mixin ("return this " ~ op ~ " IntM(v);");
  }
  int opCast(T : int)() const { return v; }
  int opCmp (const IntM rhs) const {
    if (v < rhs.v) {
      return -1;
    }
    if (v > rhs.v) {
      return 1;
    }
    return 0;
  }
  bool opEquals (const IntM rhs) const { return v == rhs.v; }
}

unittest {
  writeln ("Testing number.d ...");
  assert (gcd (4, 2) == gcd (2, 4));
  assert (gcd (4, 2) == 2);
  assert (gcd (27, 3) == 3);
  int q = 1_000_000_007;
  int x, y;
  int g = gcdext (2, q, x, y);
  assert (2 * x + q * y == g);
  assert ((IntM (2) ^^ (x - 2)).to!(int) == 1);
}