#include <vector>
#include <algorithm>
#include <array>
#include <numeric>

using namespace std;

using MoResult = int;

struct MoQuery {
  int u, v; //inclusive
  MoResult res;
  MoQuery (int _u, int _v) : u (_u), v (_v) {}
};

class MoState {
  public:
  virtual bool fastReset () const = 0;
  virtual void reset () = 0;
  //[l, r)
  virtual void add (int l, int r) = 0;
  virtual void del (int l, int r) = 0;
  virtual MoResult result () const = 0;
};

class MoRange {
  int l, r;
  bool fastReset;
  static const array<int, 4> d;
  static int64_t hilbert (const int x, const int y, const int k, const int a) {
    if (k < 0) return 0;
    const int m = ~(1 << k), o = 3 & (((x >> k) ^ 3 * (y >> k)) + a);
    const auto b = hilbert (x & m, y & m, k - 1, a + d[o]), ss = static_cast<int64_t>(1) << (2 * k);
    return ss * o + (o == 1 || o == 2 ? b : (ss - (b + 1)));
  }
  static vector<size_t> makeIdx (const vector<MoQuery>& queries, const int t) {
    const auto nq = queries.size ();
    vector<int64_t> x (nq);
    for (size_t i = 0; i < nq; ++i) {
      x[i] = hilbert (queries[i].u, queries[i].v, t, 0);
    }
    vector<size_t> idx (nq);
    iota (idx.begin (), idx.end (), 0);
    sort (idx.begin (), idx.end (), [&] (const int u, const int v) {
      return x[u] < x[v];
    });
    return idx;
  }
  void move (MoState& s, MoQuery& q) {
    if (l > r || (fastReset && (l > q.v || r < q.u))) {
      l = q.u;
      r = l - 1;
      s.reset ();
    }
    if (r < q.v) {
      s.add (r + 1, q.v + 1);
      r = q.v;
    } else if (r > q.v) {
      s.del (q.v + 1, r + 1);
      r = q.v;
    }
    if (l > q.u) {
      s.add (q.u, l);
      l = q.u;
    } else if (l < q.u) {
      s.del (l, q.u);
      l = q.u;
    }
    q.res = s.result ();
  }
  public:
  void processQueries (MoState& s, vector<MoQuery>& queries) {
    if (queries.empty ()) return;
    l = 0; r = -1; fastReset = s.fastReset ();
    auto m = max_element (queries.cbegin (), queries.cend (), [] (const MoQuery& u, const MoQuery& v) {
      return u.v < v.v;
    })->v + 1;
    auto idx = makeIdx (queries, 31 - __builtin_clz (m));
    for (const auto j : idx) {
      move (s, queries[j]);
    }
  }
};

const array<int, 4> MoRange::d = {3, 0, 0, 1};
