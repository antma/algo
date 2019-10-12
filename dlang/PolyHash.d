import std.array;
import std.conv;
import std.random;

final class PolyHash {
  private:
  static immutable int[] primes = [
    2147483647, 2147483629, 2147483587, 2147483579, 2147483563, 2147483549,
    2147483543, 2147483497, 2147483489, 2147483477, 2147483423, 2147483399,
    2147483353, 2147483323, 2147483269, 2147483249, 2147483237, 2147483179,
    2147483171, 2147483137];
  immutable int n;
  ulong[] h, d;
  public:
  static int randomP () {
    return primes[uniform (0, primes.length)];
  }
  this (in string s, int p) {
    n = s.length.to!int;
    h = uninitializedArray!(ulong[]) (n + 1);
    d = uninitializedArray!(ulong[]) (n + 1);
    h[0] = 0;
    d[0] = 1;
    foreach (i; 0 .. n) {
      h[i+1] = h[i] * p + s[i].to!int;
      d[i+1] = d[i] * p;
    }
  }
  //[l, r)
  ulong get (int l, int r) const {
    return h[r] - h[l] * d[r - l];
  }
}
