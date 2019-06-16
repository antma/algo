import std.conv;
import std.math;
import std.range;

//a*x^2 + b*x + c = 0
T[] squareEquation(T) (T a, T b, T c, T eps) {
  if (a < 0) {
    a = -a;
    c = -c;
  } else {
    b = -b;
  }
  T d = b * b - 4 * a * c;
  if (d < -eps) return [];
  T e = sqrt (d), f = 0.5 / a;
  if (d < eps) return [ b * f];
  return [ (b - e) * f, (b + e) * f];
}

unittest {
  import std.algorithm;
  import std.stdio;
  writeln ("Testing ", __FILE__, " ...");
  bool isPentagonal (long p) {
    //1.5 * n - 0.5 * n - c = 0
    real[] x = squareEquation!real (1.5, -0.5, -p.to!real, 1e-11);
    if (x.empty) return false;
    foreach (n; x) {
      long i = n.roundTo!long;
      if (i >= 0 && i * (3 * i - 1) == 2 * p) {
        return true;
      }
    }
    return false;
  }
  assert ([1, 5, 12, 22, 35, 51, 70, 92, 117, 145].all! (isPentagonal));
}
