import std.range;

int longestStrictlyIncreasingSequenceLength(T) (const T[] a) {
  immutable n = a.length;
  auto p = new T[n+2];
  int m = 1;
  p[0] = T.min;
  p[1] = T.max;
  foreach (i; a) {
    auto k = p[0 .. m].assumeSorted.lowerBound (i).length;
    if (i < p[k]) {
      p[k] = i;
      if (k == m) {
        p[++m] = T.max;
      }
    }
  }
  return m - 1;
}

unittest {
  assert (longestStrictlyIncreasingSequenceLength ([2, 7, 4, 3, 8]) == 3);
}
