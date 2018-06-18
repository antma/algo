#include <vector>
#include <algorithm>
#include <cstdint>
#include <cassert>
#include <climits>

using namespace std;

using Cost = int;
using Excess = int;
const Excess MAX_EXCESS = INT_MAX;

struct Edge {
  Cost f;
  const Cost c;
  const int v, e;
  Edge (const Cost _c, int _v, int _e) :
    f (0), c (_c), v (_v), e (_e) {
  }
};

class PushRelabelMaxFlowGraph {
  private:
  vector<vector<Edge> > edges;
  vector<vector<Edge>::iterator> current;
  vector<int> h;
  vector<Excess> e;
  vector<int> next, prev;
  const int n;
  void initPreflow () {
    h[0] = n;
    for (auto &p : edges[0]) {
      const int i = p.v;
      e[i] = p.f = p.c;
      edges[p.v][p.e].f = -p.c;
    }
  }
  void push (int i, Edge &edge) {
    const int j = edge.v;
    Cost d = edge.c - edge.f;
    if (d > e[i]) d = e[i];
    edge.f += d;
    edges[j][edge.e].f = -edge.f;
    e[i] -= d;
    e[j] += d;
  }
  void lift (int i) {
    int m = INT_MAX;
    for (const auto &p : edges[i]) {
      if (p.f < p.c && m > h[p.v]) {
        m = h[p.v];
      }
    }
    assert (m < INT_MAX);
    h[i] = m + 1;
  }
  void discharge (int i) {
    while (e[i] > 0) {
      auto l = current[i];
      if (l == edges[i].end ()) {
        lift (i);
        current[i] = edges[i].begin ();
      } else {
        auto &p = *(current[i]);
        if (p.f < p.c && h[i] == h[p.v] + 1) {
          push (i, p);
        } else {
          ++(current[i]);
        }
      }
    }
  }
  inline void addLink (int u, int v) {
    next[u] = v;
    prev[v] = u;
  }
  public:
  inline void addEdge (int i, int j, int w) {
    const int ei = edges[i].size (), ej = edges[j].size ();
    edges[i].emplace_back (w, j, ej);
    edges[j].emplace_back (0, i, ei);
  }
  //0 - sink, (n - 1) - target
  PushRelabelMaxFlowGraph (int _n) :
    edges (_n),
    current (_n),
    h (_n, 0),
    e (_n, 0),
    next (_n), prev (_n),
    n (_n)
  {

  }
  Excess maxFlow () {
    int i;
    initPreflow ();
    for (i = 1; i < n - 1; ++i) {
      next[i] = i + 1;
      prev[i] = i - 1;
      current[i] = edges[i].begin ();
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
    return e[n-1];
  }
};
