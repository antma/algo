import std.algorithm;
import std.conv;
import std.math;
import std.stdio;

import NumberTheory : genericPower, gcdext;

struct IntM(int q = 1_000_000_007) {
  alias N = IntM!q;
  private:
  int v;
  invariant () { assert (v >= 0 && v < q); }
  pure nothrow @nogc
  void fromInt (int m) {
    v = m % q;
    if (v < 0) {
      v += q;
    }
  }
  public:
  pure nothrow @nogc
  this (int m) {
    fromInt (m);
  }
  pure nothrow @nogc
  N opAssign (int m) {
    fromInt (m);
    return this;
  }
  pure nothrow @nogc
  N opUnary (string op : "-")() const {
    return N (-v);
  }
  pure nothrow @nogc
  ref N opUnary (string op : "++")() {
    if (++v >= q) {
      v -= q;
    }
    return this;
  }
  pure nothrow @nogc
  ref N opUnary (string op : "--")() {
    if (--v < 0) {
      v += q;
    }
    return this;
  }
  pure nothrow @nogc
  ref N opOpAssign (string op : "+")(in N rhs) {
    v = (v + rhs.v) % q;
    return this;
  }
  pure nothrow @nogc
  ref N opOpAssign (string op : "-")(in N rhs) {
    v = (v - rhs.v + q) % q;
    return this;
  }
  pure nothrow @nogc
  ref N opOpAssign (string op : "*")(in N rhs) {
    v = (v.to!(long) * rhs.v) % q;
    return this;
  }
  pure nothrow @nogc
  ref N opOpAssign (string op : "/")(in N rhs) {
    return this *= rhs.inversion ();
  }
  pure nothrow @nogc
  ref N opOpAssign (string op)(in int rhs) if (op == "+" || op == "-" || op == "*" || op == "/") {
    mixin ("return this " ~ op ~ "= N(rhs);");
  }
  pure nothrow @nogc
  N opBinary (string op)(in N rhs) const if (op == "+" || op == "-" || op == "*" || op == "/") {
    N t = this;
    mixin ("t " ~ op ~ "= rhs;");
    return t;
  }
  pure nothrow @nogc
  N opBinary (string op)(in int rhs) const if (op == "+" || op == "-" || op == "*" || op == "/") {
    mixin ("return this " ~ op ~ " N(rhs);");
  }
  pure nothrow @nogc
  N opBinaryRight (string op)(in int rhs) const if (op == "+" || op == "*") {
    mixin ("return this " ~ op ~ " N(rhs);");
  }
  pure nothrow @nogc
  N inversion () const {
    int x, y;
    immutable g = gcdext!int (v, q, x, y);
    assert (g == 1);
    return N (x);
  }
  pure nothrow @nogc
  N opBinary (string op : "^^")(in ulong rhs) const {
    return genericPower! ("a * b", N, ulong) (this, rhs);
  }
  pure nothrow @nogc
  int opCast(T : int)() const { return v; }
  pure nothrow @nogc
  int opCmp (const N rhs) const {
    if (v < rhs.v) {
      return -1;
    }
    if (v > rhs.v) {
      return 1;
    }
    return 0;
  }
  pure nothrow
  string toString() const { return v.text; }
  //a ^ x = this (mod q)
  pure
  int discreteLogarithm (in N a) const {
    immutable int n = ceil (sqrt (q.to!double)).to!int;
    N an = a ^^ n;
    int[int] h;
    N cur = an;
    foreach (p; 1 .. n + 1) {
      auto ptr = cur.v !in h;
      if (cur.v !in h) {
        h[cur.v] = p;
      }
      cur *= an;
    }
    cur = this;
    int res = int.max;
    foreach (q; 0 .. n + 1) {
      auto ptr = cur.v in h;
      if (ptr) {
        res = min (res, *ptr * n - q);
      }
      cur *= a;
    }
    return res != int.max ? res : -1;
  }
}

unittest {
  import std.range;
  alias N = IntM!();
  writeln ("Testing ", __FILE__, " ...");
  int q = 1_000_000_007;
  void gcdExtTest () {
    int x, y;
    int g = gcdext (2, q, x, y);
    assert (g == 1);
    assert (2 * x + q * y == g);
    assert ((N(2) ^^ (q - 1)).to!int == 1);
    assert (N(2) ^^ (q - 2) == N(x));
  }
  gcdExtTest ();
  assert (-N (1) == N (q - 1));
  auto i = N(1);
  i++;
  assert (i == N(2));
  i--;
  assert (i == N(1));
  assert ((i + 1) * (i + 1) == N(4));
  assert ((i - 2) * (i - 2) == N(1));
  assert (((i - 2) ^^ 2) == N(1));
  assert (i + i == N(2));
  assert (((N(1) / N(2)) * N (2)) == N (1));
  assert (2 * i == N(2));
  assert (2 + i == N(3));

  assert(N(2).to!int == 2);
  assert(N(1) < N(2));

  N t = 1;
  t /= N(2);
  assert (t.to!int == (q + 1) / 2);
  assert (N(1) / N(2) == t);

  t = 1;
  t /= 2;
  assert (t.to!int == (q + 1) / 2);

  //discreteLogarithm
  enum DL_TESTS = 10;
  foreach (p; [2, 3, 5, 7, 11, 17]) {
    N a = p;
    N x = 1;
    foreach (j; 0 .. DL_TESTS) {
      assert (x.discreteLogarithm (a) == j);
      x *= a;
    }
    foreach (j; iota (100, 10007 * DL_TESTS, 10007)) {
      N y = N (p) ^^ j;
      assert (y.discreteLogarithm (a) == j);
    }
  }
}
