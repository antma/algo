import std.conv;
import std.random;

//x^2 = n (mod p), p odd prime

struct GFNumber {
  uint v, p;
  GFNumber opBinary(string op)(in GFNumber rhs) const if (op == "+") {
    return GFNumber ((v + rhs.v) % p, p);
  }
  GFNumber opBinary(string op)(in GFNumber rhs) const if (op == "*") {
    return GFNumber (((v.to!ulong * rhs.v) % p).to!uint, p);
  }
  GFNumber opBinary(string op)(in uint rhs) const if (op == "*") {
    return GFNumber (((v.to!ulong * rhs) % p), p);
  }
  ref GFNumber opOpAssign(string op)(in GFNumber rhs) if (op == "*") {
    v = ((v.to!ulong * rhs.v) % p);
    return this;
  }
  GFNumber opBinary (string op)(in uint rhs) const if (op == "^^") {
    auto a = GFNumber (1U, p), b = GFNumber (v, p);
    uint y = rhs;
    while (y > 1) {
      if (y & 1) {
        a *= b;
      }
      b *= b;
      y >>>= 1;
    }
    return a *= b;
  }
  int legendreSymbol () const {
    auto r = this ^^ ((p - 1) >> 1);
    if (r.v < 2) return r.v;
    return -1;
  }
}

struct GFPair {
  GFNumber a, b;
  uint w;
  GFPair opBinary(string op)(in GFPair rhs) const if (op == "*") {
    return GFPair (a * rhs.a + b * rhs.b * w, a * rhs.b + b * rhs.a, w);
  }
  GFPair opBinary (string op)(in uint rhs) const if (op == "^^") {
    auto a = GFPair (GFNumber (1, a.p), GFNumber (0, a.p), w), b = GFPair (this.a, this.b, w);
    uint p = rhs;
    while (p > 1) {
      if (p & 1) {
        a = a * b;
      }
      b = b * b;
      p >>>= 1;
    }
    return a * b;
  }
}

int squareRootMod (uint n, uint p) {
  auto g = GFNumber (n, p);
  if (g.legendreSymbol () != 1) {
    return -1;
  }
  if ((p & 3) == 3) {
    auto e = GFNumber (n, p) ^^ ((p + 1) >> 2);
    return e.v;
  }
  auto mn = GFNumber (p - n, p);
  while (1) {
    auto a = GFNumber (uniform (0, p - 1), p), b = a * a + mn;
    if (b.legendreSymbol () < 0) {
      auto d = GFPair (a, GFNumber (1, p), b.v) ^^ ((p + 1) >> 1);
      return d.a.v;
    }
  }
}
