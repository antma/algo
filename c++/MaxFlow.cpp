#include <algorithm>
#include <limits>
#include <vector>

using namespace std;

template<typename C, typename E> class PushRelabelMaxFlowGraph {
  struct DListEntry {
    int prev, next;
  };
  struct Edge {
    C f;
    const C c;
    const int v, e;
    Edge (C _c, int _v, int _e) :
      f (0),
      c (_c),
      v (_v),
      e (_e)
    {
    }
  };
  const int n;
  int maxh;
  vector<vector<Edge> > edges;
  vector<int> current, h, nl, gc;
  vector<E> e;
  vector<DListEntry> dl;
  void addLink (int u, int v) {
    dl[u].next = v;
    dl[v].prev = u;
  }
  void insert (int ht, int v) {
    addLink (dl[n + ht].prev, v);
    addLink (v, n + ht);
  }
  void remove (int i) {
    addLink (dl[i].prev, dl[i].next);
  }
  void initPreflow () {
    for (int i = n; i < (int) dl.size (); ++i) {
      dl[i].prev = dl[i].next = i;
    }
    h[0] = n;
    for (auto& p : edges[0]) {
      const auto i = p.v;
      if (!e[i] && i && i < n - 1) {
        insert (0, i);
      }
      e[i] += p.f = p.c;
      edges[p.v][p.e].f = -p.c;
    }
    gc[0] = n - 1;
  }
  void push (int i, Edge &edge) {
    const auto j = edge.v;
    E d = edge.c - edge.f;
    if (d > e[i]) d = e[i];
    edge.f += d;
    edges[j][edge.e].f = -edge.f;
    e[i] -= d;
    e[j] += d;
  }
  void lift (int i) {
    int m = numeric_limits<int>::max();
    for (const auto& p : edges[i]) {
      if (p.f < p.c && m > h[p.v]) {
        m = h[p.v];
      }
    }
    h[i] = m + 1;
  }
  void discharge (int i) {
    while (e[i] > 0) {
      if (current[i] == (int) edges[i].size ()) {
        current[i] = 0;
        if (!(--gc[maxh]) && maxh && maxh < n) {
          remove (i);
          h[i] = n + 1;
          for (int k = 1; k < n; ++k) {
            if (h[k] > maxh && h[k] <= n) {
              --gc[h[k]];
              h[k] = n + 1;
            }
          }
          maxh = nl[maxh];
          break;
        } else {
          lift (i);
          remove (i);
          if (h[i] < n) {
            ++gc[h[i]];
            insert (h[i], i);
            nl[h[i]] = (dl[maxh + n].next >= n) ? nl[maxh] : maxh;
            maxh = h[i];
          } else {
            if (dl[maxh + n].next >= n) {
              maxh = nl[maxh];
            }
            break;
          }
        }
      } else {
        auto &p = edges[i][current[i]];
        const auto j = p.v;
        if (h[i] == h[j] + 1 && p.f < p.c) {
          const bool aj = e[j] > 0;
          push (i, p);
          if (!aj && j && j < n - 1 && e[j] > 0) {
            insert (maxh - 1, j);
            if (nl[maxh] != maxh - 1) {
              nl[maxh-1] = nl[maxh];
              nl[maxh] = maxh - 1;
            }
          }
          if (e[i] <= 0) {
            remove (i);
            if (dl[maxh + n].next >= n) {
              maxh = nl[maxh];
            }
          }
        } else {
          ++current[i];
        }
      }
    }
  }
  public:
  void addEdge (int i, int j, C w1, C w2) {
    const auto ei = (int) edges[i].size (), ej = (int) edges[j].size ();
    edges[i].emplace_back (w1, j, ej);
    edges[j].emplace_back (w2, i, ei);
  }
  //0 - sink, (n - 1) - target
  PushRelabelMaxFlowGraph (int _n) :
    n (_n),
    edges (n),
    current (n),
    h (n), nl (n, -1), gc (n, 0),
    e (n),
    dl (2 * n)
  {
  }
  E maxFlow () {
    initPreflow ();
    for (int i = 1; i < n - 1; ++i) {
      current[i] = 0;
    }
    maxh = 0;
    while (maxh >= 0) {
      int v = dl[maxh + n].next;
      if (v >= n) {
        break;
      }
      discharge (v);
    }
    return e[n-1];
  }
};
