import std.algorithm;
import std.conv;
import std.math;
import std.range;
import std.traits;

int tcmp(T) (in T x, in T y) if (isIntegral!T) {
  if (x < y) {
    return -1;
  }
  if (x > y) {
    return 1;
  }
  return 0;
}

struct Point(T) {
  T x, y;
  int opCmp (in Point!T rhs) const {
    int cy = tcmp!T (y, rhs.y);
    if (cy) {
      return cy;
    }
    return tcmp!T (x, rhs.x);
  }
  ref Point!T opOpAssign (string op)(in Point!T rhs) if (op == "+" || op == "-") {
    mixin ("x " ~ op ~ "= rhs.x;");
    mixin ("y " ~ op ~ "= rhs.y;");
    return this;
  }
  Point!T opBinary (string op)(in Point!T v) const if (op == "+" || op == "-") {
    mixin ("return Point (x " ~ op ~ " v.x, y " ~ op ~ " v.y);");
  }
  T dotProduct (in Point!T rhs) const {
    return x * rhs.x + y * rhs.y;
  }
  T crossProduct (in Point!T rhs) const {
    return x * rhs.y - y * rhs.x;
  }
}

bool cmpAngle(T) (in Point!T a, in Point!T b) {
  debug stderr.writefln ("cmpAngle(%s, %s)", a, b);
  int z = tcmp (a.y * b.x, b.y * a.x);
  if (z < 0) {
    return true;
  }
  if (z > 0) {
    return false;
  }
  return tcmp (a.x * a.x + a.y * a.y, b.x * b.x + b.y * b.y) > 0;
}

bool left(T) (ref in Point!T a, ref in Point!T b, ref in Point!T c) {
  return tcmp ((c.x - a.x) * (b.y - a.y), (b.x - a.x) * (c.y - a.y)) < 0;
}

Point!T[] convexHull(T) (const Point!(T)[] v) {
  Point!T[] h;
  immutable int n = v.length.to!int;
  auto x = v.dup;
  if (v.empty) {
    return x;
  }
  immutable me = minElement (x);
  x.length -= x.filter!(a => a != me).map!(a => a - me).copy(x).length;
  x.sort!(cmpAngle);
  x.length -= x.uniq! ((a, b) => !tcmp (a.y * b.x, b.y * a.x)).copy (x).length;
  h ~= Point!T (0, 0);
  foreach (i; 0 .. min (2, x.length)) {
    h ~= x[i];
  }
  auto m = h.length;
  foreach (i; 2 .. x.length) {
    while (!left (h[m - 2], h[m - 1], x[i])) {
      --m;
    }
    if (m < h.length) {
      h[m] = x[i];
    } else {
      h ~= x[i];
    }
    ++m;
  }
  foreach (i; 0 .. m) {
    h[i] += me;
  }
  return h[0 .. m].dup;
}

