#include <vector>
#include <algorithm>
#include <cstdint>
#include <cassert>
#include <climits>

using namespace std;

class PushRelabelMaxFlowGraph {
  private:
  vector<vector<pair<int, int> > > edges;
  vector<int> f;
  vector<int> h;
  vector<int64_t> e;
  vector<vector<pair<int, int>>::const_iterator> current;
  vector<int> next, prev;
  int n;
  void buildEdges () {
    for (int i = 0; i < n; ++i) for (int j = i + 1; j < n; ++j) {
      if (f[i * n + j] > 0 || f[j * n + i] > 0) {
        edges[i].emplace_back (j, f[i * n + j]);
        edges[j].emplace_back (i, f[j * n + i]);
        f[i * n + j] = f[j * n + i] = 0;
      }
    }
  }
  void initPreflow () {
    h[0] = n;
    for (const auto &p : edges[0]) {
      const int i = p.first;
      e[i] = f[i] = p.second;
      f[i * n] = -p.second;
    }
  }
  void push (int i, int j, int cij) {
    int ij = i * n + j;
    int d = cij - f[ij];
    if (d > e[i]) d = e[i];
    f[ij] += d;
    f[j * n + i] = -f[ij];
    e[i] -= d;
    e[j] += d;
  }
  void lift (int i) {
    int m = INT_MAX;
    for (const auto &p : edges[i]) {
      const int j = p.first;
      if (f[i * n + j] < p.second) {
        if (m > h[j]) m = h[j];
      }
    }
    assert (m < INT_MAX);
    h[i] = m + 1;
  }
  void discharge (int i) {
    while (e[i] > 0) {
      auto l = current[i];
      if (l == edges[i].cend ()) {
        lift (i);
        current[i] = edges[i].cbegin ();
      } else {
        const auto &p = *(current[i]);
        const int j = p.first;
        if (f[i * n + j] < p.second && h[i] == h[j] + 1) push (i, j, p.second);
        else ++(current[i]);
      }
    }
  }
  inline void addLink (int u, int v) {
    next[u] = v;
    prev[v] = u;
  }
  public:
  inline void addEdge (int i, int j, int w) {
    f[i * n + j] = w;
  }
  //0 - sink, (n - 1) - target
  PushRelabelMaxFlowGraph (int _n) :
    edges (_n),
    f (_n * _n, 0),
    h (_n, 0),
    e (_n, 0),
    current (_n),
    next (_n), prev (_n),
    n (_n)
  {

  }
  int64_t maxFlow () {
    int i;
    buildEdges ();
    initPreflow ();
    for (i = 1; i < n - 1; ++i) {
      next[i] = i + 1;
      prev[i] = i - 1;
      current[i] = edges[i].cbegin ();
    }
    next[0] = 1;
    addLink (n - 2, 0);
    i = 1;
    while (i > 0) {
      const int old_height = h[i];
      discharge (i);
      if (h[i] > old_height) {
        //move to front
        addLink (prev[i], next[i]);
        addLink (i, next[0]);
        addLink (0, i);
      }
      i = next[i];
    }
    int64_t res = 0;
    for (i = 1; i < n; ++i) res += f[i];
    return res;
  }
};

