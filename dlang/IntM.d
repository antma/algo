import std.algorithm;
import std.conv;
import std.math;
import std.stdio;

import NumberTheory : genericPower, gcdext;

struct IntM(int q = 1_000_000_007) {
  alias N = IntM!q;
  int v;
  private void fromInt (int m) pure nothrow @nogc {
    v = m % q;
    if (v < 0) {
      v += q;
    }
  }
  this (int m) pure nothrow @nogc {
    fromInt (m);
  }
  N opAssign (int m) pure nothrow @nogc {
    fromInt (m);
    return this;
  }
  N opUnary (string op : "-")() const pure nothrow @nogc {
    return N ((q - v) % q);
  }
  ref N opUnary (string op : "++")() pure nothrow @nogc {
    if (++v >= q) {
      v -= q;
    }
    return this;
  }
  ref N opUnary (string op : "--")() pure nothrow @nogc {
    if (--v < 0) {
      v += q;
    }
    return this;
  }
  ref N opOpAssign (string op : "+")(in N rhs) pure nothrow @nogc {
    v += rhs.v;
    v %= q;
    return this;
  }
  ref N opOpAssign (string op : "-")(in N rhs) pure nothrow @nogc {
    v -= rhs.v;
    v %= q;
    return this;
  }
  ref N opOpAssign (string op : "*")(in N rhs) pure nothrow @nogc {
    v = ((v.to!(long)) * rhs.v.to!(long)) % q;
    return this;
  }
  N opBinary (string op : "+")(in N rhs) const pure nothrow @nogc {
    return N ( (v + rhs.v) % q);
  }
  N opBinary (string op : "-")(in N rhs) const pure nothrow @nogc {
    return N ( (v - rhs.v) % q);
  }
  N opBinary (string op : "*")(in N rhs) const pure nothrow @nogc {
    return N (((v.to!(long)) * rhs.v.to!(long)) % q);
  }
  N opBinary (string op : "/")(in N rhs) const pure nothrow @nogc {
    return this * rhs.inversion ();
  }
  N inversion () const pure @nogc {
    int x, y;
    immutable g = gcdext!int (v, q, x, y);
    assert (g == 1);
    return N (x);
  }
  N opBinary (string op : "^^")(in long rhs) const pure nothrow @nogc {
    return genericPower! ("a * b", N, ulong) (this, rhs);
  }
  N opBinary (string op)(in int v) const pure nothrow @nogc if (op == "+" || op == "-" || op == "*" || op == "/") {
    mixin ("return this " ~ op ~ " N(v);");
  }
  int opCast(T : int)() const pure nothrow @nogc { return v; }
  int opCmp (const N rhs) const pure nothrow @nogc {
    if (v < rhs.v) {
      return -1;
    }
    if (v > rhs.v) {
      return 1;
    }
    return 0;
  }
  bool opEquals (const N rhs) const pure nothrow @nogc { return v == rhs.v; }
  string toString() const pure nothrow { return ((v < 0) ? v + q : v).text; }
  //a ^ x = this (mod q)
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
  assert (i + i == N(2));
  assert (((N(1) / N(2)) * N (2)) == N (1));

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
