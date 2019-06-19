import std.algorithm;
import std.conv;
import std.range;

//https://oeis.org/A001318
auto generalizedPentagonalNumbers () {
  return roundRobin (only (0UL),
    iota (1UL, ulong.max).map! (m => (m & 1) ? m * ((3 * m - 1) >> 1) : (m >> 1) * (3 * m - 1)).cache,
    iota (1UL, ulong.max).map! (m => (m & 1) ? m * ((3 * m + 1) >> 1) : (m >> 1) * (3 * m + 1)).cache);
}

unittest {
  assert (generalizedPentagonalNumbers().take(60).equal(
  [0,1,2,5,7,12,15,22,26,35,40,51,57,70,77,92,100,117,126,145,155,176,187,210,222,247,260,287,301,330,345,376,392,425,442,477,495,532,551,590,610,651,672,715,737,782,805,852,876,925,950,1001,1027,1080,1107,1162,1190,1247,1276,1335]));
}

//https://oeis.org/A000041
auto partitionNumbers(T) () {
  struct R {
    private static T[] a;
    private static typeof(generalizedPentagonalNumbers()) gpn;
    private size_t idx;
    enum empty = false;
    @property T front () { return a[idx]; }
    void popFront () {
      immutable n = a.length;
      if (++idx >= n) {
        T r;
        foreach (i, v; gpn.enumerate ()) {
          if (v > n) break;
          if ((i >>> 1) & 1) {
            r -= a[n - v];
          } else {
            r += a[n - v];
          }
        }
        a ~= r;
      }
      assert (idx < a.length);
    }
    static this() {
      gpn = generalizedPentagonalNumbers ().dropOne;
      a = [ 1.to!T ];
    }
  }
  return R ();
}

unittest {
  assert (partitionNumbers!uint().take(50).equal(
  [1,1,2,3,5,7,11,15,22,30,42,56,77,101,135,176,231,297,385,490,627,792,1002,1255,1575,1958,2436,3010,3718,4565,5604,6842,8349,10143,12310,14883,17977,21637,26015,31185,37338,44583,53174,63261,75175,89134,105558,124754,147273,173525]));
}
