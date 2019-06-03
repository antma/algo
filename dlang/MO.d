import std.algorithm;
import std.conv;
import std.math;
import std.typecons;

struct MoQuery {
  int u;
  int v;
}

class MoRange(R) {
  private:
  int l, r;
  final void move (MoQuery q) {
    if (l > r || l > q.v || r < q.u) {
      l = q.u;
      r = q.u - 1;
      clear ();
    }
    if (r < q.v) {
      add (r + 1, q.v + 1);
      r = q.v;
    } else if (r > q.v) {
      del (q.v + 1, r + 1);
      r = q.v;
    }
    if (l > q.u) {
      add (q.u, l);
      l = q.u;
    } else if (l < q.u) {
      del (l, q.u);
      l = q.u;
    }
  }
  protected:
  abstract void clear ();
  abstract void add (int a, int e);
  abstract void del (int a, int e);
  abstract R query () const;
  public:
  final R[] processQueries (in MoQuery[] queries) {
    l = 0;
    r = -1;
    immutable nq = queries.length;
    immutable n = queries.fold! ( (x, y) => max (x, y.v)) (int.min);
    immutable k = max (1, (sqrt ((n+1).to!double) + 0.5).to!int);
    alias P = Tuple!(int, int);
    auto x = new P[nq];
    foreach (int i, ref q; queries) {
      int o = q.u / k;
      if (o & 1) {
        x[i][0] = (o << 20) - q.v;
      } else {
        x[i][0] = (o << 20) + q.v;
      }
      x[i][1] = i;
    }
    x.sort!"a[0] < b[0]";
    auto res = new R[nq];
    foreach (i, ref p; x) {
      int j = p[1];
      move (queries[j]);
      res[j] = query ();
    }
    return res;
  }
}

unittest {
  class MoTest : MoRange!int {
  
  }
}
