import std.algorithm;
import std.array;
import std.conv;
import std.range;

import SegmentTree : SegmentTree;

struct Edge {
  int u, v;
  int neighbour (in int i) const {
    return i ^ u ^ v;
  }
}

struct LCANode {
  int h;
  int best;
}

pure nothrow @nogc
LCANode f (in LCANode x, in LCANode y) {
  return (x.h <= y.h) ? x : y;
}

alias PtrIEdge = immutable (Edge)*;
alias LCASegmentTree = SegmentTree!(LCANode, f, LCANode (int.max));

class HeavyLightDecompositionTree {
  struct Node {
    int u;
    int v;
    int first;
    int parent;
    int h;
    int subtree_size;
    int path;
    int k;
    bool has_heavy;
  }
  private:
  immutable Edge[] edges;
  PtrIEdge[][] e;
  LCANode[] order;
  LCASegmentTree stLCA;
  int[][] paths;
  Node[] a;
  int timestamp;
  bool edge_disjoint;
  final void go (int i, int depth, int parent) {
    a[i].h = depth;
    a[i].parent = parent;
    a[i].u = timestamp++;
    a[i].subtree_size = 1;
    a[i].first = order.length.to!int;
    order ~= LCANode (depth, i);
    int max_child_size;
    foreach (q; e[i]) {
      int j = q.neighbour (i);
      if (a[j].parent == -2) {
        go (j, depth + 1, i);
        int cs = a[j].subtree_size;
        if (max_child_size < cs) {
          max_child_size = cs;
        }
        a[i].subtree_size += cs;
        order ~= LCANode (depth, i);
      }
    }
    a[i].has_heavy = 2 * max_child_size >= a[i].subtree_size;
    a[i].v = timestamp++;
  }
  int lca (in int i, in int j) const {
    int x = a[i].first, y = a[j].first;
    if (x > y) {
      swap (x, y);
    }
    return stLCA.reduce (x, y + 1).best;
  }
  void updateUpEdgeDisjoint (in int u, in int v, void delegate (in int, in int, in int) op) {
    int l = a[u].h - a[v].h;
    if (!l) return;
    int x = u;
    while (true) {
      const k = a[x].path;
      const i = a[x].k;
      const t = (paths[k].length - i - 1).to!int;
      const m = min (t, l);
      op (k, i, i + m);
      l -= m;
      if (!l) break;
      x = paths[k].back;
    }
  }
  public:
  void updateEdgeDisjoint (in int u, in int v, void delegate (in int pathid, in int u, in int v) op) {
    assert (edge_disjoint);
    const w = lca (u, v);
    if (v == w) {
      updateUpEdgeDisjoint (u, w, op);
    } else if (u == w) {
      updateUpEdgeDisjoint (v, w, op);
    } else {
      updateUpEdgeDisjoint (u, w, op);
      updateUpEdgeDisjoint (v, w, op);
    }
  }

  this (in int n, in Edge[] _edges, in int root, in bool edge_disjoint) in {
    assert (_edges.length == n - 1);
  } body {
    edges = _edges.idup;
    auto deg = new int[n];
    foreach (p; edges) {
      ++deg[p.u];
      ++deg[p.v];
    }
    e = new PtrIEdge[][n];
    a = new Node[n];
    foreach (i; 0 .. n) {
      e[i].reserve (deg[i]);
      a[i].parent = -2;
      a[i].path = -1;
    }
    foreach (i; 0 .. edges.length) {
      e[edges[i].u] ~= &edges[i];
      e[edges[i].v] ~= &edges[i];
    }
    timestamp = 0;
    order.reserve (2 * n);
    go (root, 0, -1);
    stLCA = new LCASegmentTree (order);
    order = null;
    foreach (i; 0 .. n) {
      if (!a[i].has_heavy) {
        int[] path;
        int x = i;
        path ~= x;
        while (true) {
          int y = a[x].parent;
          if (y < 0) {
            break;
          }
          const light_edge = 2 * a[x].subtree_size < a[y].subtree_size;
          if (!edge_disjoint && light_edge) break;
          path ~= y;
          if (light_edge) break;
          x = y;
        }
        paths ~= path;
      }
    }
    foreach (i, p; paths) {
      int path = i.to!int;
      auto q = p;
      if (edge_disjoint) q = dropBackOne (q);
      foreach (k, x; q) {
        if (a[x].path < 0) {
          a[x].path = path;
          a[x].k = k.to!int;
        } else {
          assert (true);
        }
      }
    }
    if (!edge_disjoint) {
      assert (a.all! ((const ref x) => x.path >= 0));
    }
    this.edge_disjoint = edge_disjoint;
  }
}

