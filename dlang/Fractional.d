import std.algorithm;
import std.bigint;
import std.conv;
import std.format;
import std.range;
//phobos gcd isn't worked with BigInt type
//https://issues.dlang.org/show_bug.cgi?id=19514
BigInt _gcd (BigInt a, BigInt b) {
  if (a < 0) {
    a *= -1;
  }
  if (b < 0) {
    b *= -1;
  }
  if (a < b) {
    swap (a, b);
  }
  while (b) {
    a %= b;
    swap (a, b);
  }
  return a;
}

struct Rational {
  BigInt numerator, denominator;
  private void normalize () {
    auto g = _gcd (numerator, denominator);
    if (g > 1) {
      numerator /= g;
      denominator /= g;
    }
  }
  this (BigInt n, BigInt d) {
    if (d > 0) {
      numerator = n;
      denominator = d;
    } else {
      assert (d < 0);
      numerator = -n;
      denominator = -d;
    }
    normalize ();
  }
  Rational inverse () const {
    return Rational (denominator, numerator);
  }
  ref Rational opOpAssign (string op)(in Rational rhs) if (op == "+" || op == "-") {
    auto g = _gcd (denominator, rhs.denominator.to!BigInt);
    if (g > 1) {
      auto t = rhs.denominator / g;
      mixin ("numerator = numerator * t " ~ op ~ " rhs.numerator * (denominator / g);");
      denominator *= t;
    } else {
      mixin ("numerator = numerator * rhs.denominator " ~ op ~ " rhs.numerator * denominator;");
      denominator *= rhs.denominator;
    }
    normalize ();
    return this;
  }
  ref Rational opOpAssign (string op : "*")(in Rational rhs) {
    numerator *= rhs.numerator;
    denominator *= rhs.denominator;
    normalize ();
    return this;
  }
  ref Rational opOpAssign (string op : "/")(in Rational rhs) {
    numerator *= rhs.denominator;
    denominator *= rhs.numerator;
    if (denominator < 0) {
      numerator *= -1;
      denominator *= -1;
    } else {
      assert (denominator > 0);
    }
    normalize ();
    return this;
  }
  Rational opBinary (string op)(in Rational rhs) const if (op == "+" || op == "-" || op == "*" || op == "/") {
    Rational t;
    t.numerator = numerator;
    t.denominator = denominator;
    mixin ("return t " ~ op ~ "= rhs;");
  }
  string toString () const {
    return format! "%s / %s" (numerator, denominator);
  }
}

unittest {
  import std.stdio;
  writeln ("Testing ", __FILE__, " ...");
  auto _one = BigInt (1);
  auto _two = BigInt (2);
  auto one = Rational (_one, _one);
  auto two = Rational (_two, _one);
  auto r = recurrence! ( (x, n) => two + (one / x[n-1]) ) (Rational (BigInt (5), _two)).map! (x => (x - one).text).take (8);
  assert (equal (r, [ "3 / 2", "7 / 5", "17 / 12", "41 / 29", "99 / 70", "239 / 169", "577 / 408", "1393 / 985"]));
}
