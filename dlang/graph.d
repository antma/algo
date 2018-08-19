import std.typecons;

class UndirectedGraph {
  immutable int n;
  int[][] edges;
  this (int _n) {
    n = _n;
    edges = new int[][n];
  }
  final addEdge (int i, int j) {
    edges[i] ~= j;
    edges[j] ~= i;
  }
}

class WeightedUndirectedGraph(W) {
  immutable int n;
  alias E = Tuple!(int,W);
  E[][] edges;
  this (int _n) {
    n = _n;
    edges = new E[][n];
  }
  final addEdge (int i, int j, W weight) {
    edges[i] ~= tuple (j, weight);
    edges[j] ~= tuple (i, weight);
  }
}
