import std.algorithm, std.range;

class DisjointSet {
  private:
    int [] p, h;
    int n;
  public:
  this (int _n) {
    n = _n;
    p = iota (0, n).array;
    h = new int[n];
  }
  int find_set (int x) pure nothrow @nogc {
    if (p[x] == x) {
      return x;
    }
    return p[x] = find_set (p[x]);
  }
  void merge (int i, int j) pure nothrow @nogc {
    i = find_set (i);
    j = find_set (j);
    if (i != j) {
      if (h[i] < h[j]) {
        p[i] = j;
      } else if (h[i] > h[j]) {
        p[j] = i;
      } else {
        p[i] = j;
        ++h[j];
      }
    }
  }
  int biggest_set_size () pure {
    auto c = new int[n];
    foreach (i; 0 .. n) {
      ++c[find_set (i)];
    }
    return c.reduce! (max);
  }
}

