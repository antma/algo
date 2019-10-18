import std.algorithm;
import std.conv;
import std.range;

struct DListEntry {
  int prev, next;
}

struct MaxFlowEdge(C) {
  C f;
  immutable C c;
  immutable int v, e;
  this (in C _c, in int _v, in int _e) {
    c = _c;
    v = _v;
    e = _e;
  }
}

final class PushRelabelMaxFlowGraph(C,E) {
  alias Edge = MaxFlowEdge!C;
  private:
  Edge[][] edges;
  Edge[][] current;
  int[] h, nl;
  E[] e;
  DListEntry[] dl;
  int[] gc;
  immutable int n;
  int maxh;
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
    foreach (i; n .. dl.length.to!int) {
      dl[i].prev = dl[i].next = i;
    }
    h[0] = n;
    foreach (ref p; edges[0]) {
      immutable i = p.v;
      if (!e[i] && i && i < n - 1) {
        insert (0, i);
      }
      e[i] += p.f = p.c;
      edges[p.v][p.e].f = -p.c;
    }
    gc[0] = n - 1;
  }
  void push (int i, ref Edge edge) {
    immutable j = edge.v;
    E d = edge.c - edge.f;
    if (d > e[i]) d = e[i];
    edge.f += d;
    edges[j][edge.e].f = -edge.f;
    e[i] -= d;
    e[j] += d;
  }
  void lift (int i) {
    int m = int.max;
    foreach (const p; edges[i]) {
      if (p.f < p.c && m > h[p.v]) {
        m = h[p.v];
      }
    }
    h[i] = m + 1;
  }
  void discharge (int i) {
    //int nh = INT_MAX;
    while (e[i] > 0) {
      if (current[i].empty) {
        current[i] = edges[i];
        if (!(--gc[maxh]) && maxh && maxh < n) {
          remove (i);
          h[i] = n + 1;
          foreach (k; 1 .. n) {
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
        auto p = &current[i][0];
        immutable j = p.v;
        if (h[i] == h[j] + 1 && p.f < p.c) {
          immutable aj = e[j] > 0;
          push (i, *p);
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
          current[i].popFront ();
        }
      }
    }
  }
  public:
  void addEdge (int i, int j, C w1, C w2) {
    immutable ei = edges[i].length.to!int, ej = edges[j].length.to!int;
    edges[i] ~= Edge (w1, j, ej);
    edges[j] ~= Edge (w2, i, ei);
  }
  //0 - sink, (n - 1) - target
  this (int _n) {
    n = _n;
    edges = new Edge[][n];
    current = new Edge[][n];
    h = new int[n];
    nl = uninitializedArray!(int[]) (n);
    nl[] = -1;
    e = new E[n];
    dl = uninitializedArray!(DListEntry[]) (2 * n);
    gc = new int[n];
  }
  E maxFlow () {
    initPreflow ();
    foreach (i; 1 .. n - 1) {
      current[i] = edges[i];
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
}

unittest {
  auto g = new PushRelabelMaxFlowGraph!(int, int) (3);
  g.addEdge (0, 1, 1, 1);
  g.addEdge (0, 2, 1, 1);
  g.addEdge (1, 2, 1, 1);
  assert (g.maxFlow () == 2);
}
