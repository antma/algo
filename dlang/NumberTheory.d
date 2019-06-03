import std.algorithm;
import std.range;

int[] sieveArray (int n) {
  auto a = chain (only (0), iota (1, n + 1).map! (i => (i & 1) ? i : 2)).array;
  for (int p = 3; p * p <= n; p += 2) {
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
  assert (equal (sieveArray (7), [0, 1, 2, 3, 2, 5, 2, 7]));
}
