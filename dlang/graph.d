import std.container;

class Graph {
  int n;
  SList!int [] g;
  this (int n_) {
    n = n_;
    g = new SList!int [n];
  }
  void add_edge (int i, int j) {
    g[i].insert (j);
  }
  void bfs (int start, int[] d, int[] parent) {
    auto q = new DList!(int);
    d[] = -1;
    d[start] = 0;
    parent[start] = -1;
    q.insertBack (start);
    while (!q.empty()) {
      immutable i = q.front ();
      q.removeFront ();
      foreach (j; g[i]) {
        if (d[j] < 0) {
          d[j] = d[i] + 1;
          parent[j] = i;
          q.insertBack (j);
        }
      }
    }
  }
}
