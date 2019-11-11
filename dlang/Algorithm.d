//generic algorithms on integral sequences
import std.algorithm;
import std.range;
import std.typecons;
import std.traits;

//LIS
int longestIncreasingSequenceLength(R, bool strict=true) (R range)
  if (isInputRange!R && isIntegral!(ElementType!R) && hasLength!R) {
  alias T = ElementType!R;
  auto p = new T[range.length];
  int m = 1;
  p[0] = T.min;
  p[1] = T.max;
  foreach (i; range) {
    static if (strict) {
      immutable j = i;
    } else {
      immutable j = i + 1;
    }
    auto k = p[0 .. m].assumeSorted.lowerBound (j).length;
    if (i < p[k]) {
      p[k] = i;
      if (k == m) {
        p[++m] = T.max;
      }
    }
  }
  return m - 1;
}

//Kadane's algorithm
long maximumSubArraySum(R) (R range) if (isInputRange!R && isIntegral!(ElementType!R)) {
  alias T = ElementType!R;
  alias S = Tuple! (long, "best", long, "cur");
  S next (in S s, in T x) {
    S t;
    t.cur = s.cur + x;
    t.best = max (s.best, t.cur);
    if (t.cur < 0) t.cur = 0;
    return t;
  }
  return reduce!next (tuple!("best", "cur") (long.min, 0L), range).best;
}

unittest {
  import std.stdio;
  writeln ("Testing ", __FILE__, " ...");
  assert (longestIncreasingSequenceLength ([2, 7, 4, 3, 8]) == 3);
  assert (maximumSubArraySum ([5, -2 , 1]) == 5L);
}

