import std.array;
import std.conv;
import std.typecons;
import DisjointSet;

int[] tarjanOflineLCA (int root, in int[][] childs, in Tuple!(int, int)[] v) {
  immutable n = childs.length.to!int;
  auto anc = new int[n];
  auto color = new bool[n];
  auto es = new int[][n];
  auto ds = new DisjointSet (n);
  foreach (int i, ref p; v) {
    es[p[0]] ~= i;
    es[p[1]] ~= i;
  }
  auto r = uninitializedArray!(int[])(v.length);
  void lca (int i) {
    anc[ds.findSet (i)] = i;
    foreach (j; childs[i]) {
      lca (j);
      ds.merge (i, j);
      anc[ds.findSet (i)] = i;
    }
    color[i] = true;
    foreach (k; es[i]) {
      immutable j = (v[k][0] != i) ? v[k][0] : v[k][1];
      if (color[j]) {
        r[k] = anc[ds.findSet (j)];
      }
    }
  }
  lca (root);
  return r;
}
