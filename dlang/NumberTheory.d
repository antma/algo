import std.algorithm;
import std.conv;
import std.math;
import std.range;

int[] sieveArray (int n) {
  auto a = chain (only (0), iota (1, n + 1).map! (i => (i & 1) ? i : 2)).array;
  foreach (p; iota (3, (sqrt(n.to!double) + 1e-9).to!int + 1, 2)) {
    if (a[p] == p) {
      foreach (o; iota (p * p, n + 1, 2 * p)) {
        if (a[o] == o) {
          a[o] = p;
        }
      }
    }
  }
  return a;
}

unittest {
  import std.stdio;
  writeln ("Testing ", __FILE__, " ...");
  assert (equal (sieveArray (9), [0, 1, 2, 3, 2, 5, 2, 7, 2, 3]));
}
