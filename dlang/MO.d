import std.algorithm;
import std.array;
import core.bitop;

alias MoResult = int;

struct MoQuery {
  int u, v;//inclusive
  MoResult res;
}

abstract class MoState {
  public:
  @property
  abstract bool fastReset () const;
  abstract void reset ();
  //[l, r)
  abstract void addLeft (int l, int r);
  abstract void addRight (int l, int r);
  abstract void delLeft (int l, int r);
  abstract void delRight (int l, int r);
  abstract MoResult result () const;
}

struct MoRange {
  int l, r;
  bool fastReset;
  static immutable d = [3, 0, 0, 1];
  static long hilbert (const int x, const int y, const int k, const int a) {
    if (k < 0) return 0L;
    immutable int m = ~(1 << k), o = 3 & (((x >> k) ^ 3 * (y >> k)) + a);
    immutable long b = hilbert (x & m, y & m, k - 1, a + d[o]), ss = 1L << (2 * k);
    return ss * o + (o == 1 || o == 2 ? b : (ss - (b + 1)));
  }
  static size_t[] makeIdx (in MoQuery[] queries, const int t) {
    immutable nq = queries.length;
    auto x = uninitializedArray!(long[])(nq);
    foreach (i, const ref q; queries) {
      x[i] = hilbert (q.u, q.v, t, 0);
    }
    auto idx = uninitializedArray!(size_t[])(nq);
    makeIndex (x, idx);
    return idx;
  }
  void move (MoState s, ref MoQuery q) {
    if (l > r) {
      l = q.u;
      r = l - 1;
      s.reset ();
    } else if (l > q.v || r < q.u) {
      if (fastReset) {
        s.reset ();
      } else {
        s.delLeft (l, r + 1);
      }
      l = q.u;
      r = l - 1;
    }
    if (r < q.v) {
      s.addRight (r + 1, q.v + 1);
      r = q.v;
    } else if (r > q.v) {
      s.delRight (q.v + 1, r + 1);
      r = q.v;
    }
    if (l > q.u) {
      s.addLeft (q.u, l);
      l = q.u;
    } else if (l < q.u) {
      s.delLeft (l, q.u);
      l = q.u;
    }
    q.res = s.result ();
  }
  void processQueries (MoState s, MoQuery[] queries) {
    l = 0; r = -1; fastReset = s.fastReset;
    auto idx = makeIdx (queries, bsr (reduce!max(0, queries.map!(t => t.v)) + 1));
    foreach (j; idx) {
      move (s, queries[j]);
    }
  }
}
