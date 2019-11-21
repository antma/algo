import std.conv;
import std.traits;

auto sumGeometricSeries(T,U) (in T a, in T q, U l)
  if (isUnsigned!U) {
  T w = q;
  --w;
  if (w == 0.to!T) {
    return a * l.to!T;
  }
  T r = q ^^ l;
  --r;
  return a * r / w;
}

unittest {
  import IntM, std.stdio;
  writeln ("Testing ", __FILE__, " ...");
  assert (sumGeometricSeries (1, 2, 4U) == 15);
  alias N = IntM.IntM!1_000_000_007;
  assert (sumGeometricSeries (N(1), N(2), 4U) == N(15));
  assert (sumGeometricSeries (N(1), N(1), 1UL << 60) == N(1L << 60));
}
