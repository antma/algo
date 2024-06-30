class DisjointSet {
  private int[] p;
  private int[] h;
  public DisjointSet(DisjointSet dsu) {
    p = (int[]) dsu.p.Clone();
    h = (int[]) dsu.h.Clone();
  }
  public DisjointSet(int n) {
    this.p = new int[n];
    this.h = new int[n];
    for (int i = 0; i < n; i++) p[i] = i;
  }
  public int FindSet(int x) {
    int y = p[x];
    if (y == x) {
      return x;
    }
    p[x] = FindSet(y);
    return p[x];
  }
  public bool Merge(int x, int y) {
    int i = FindSet(x);
    int j = FindSet(y);
    if (i != j) {
      if (h[i] < h[j]) {
        p[i] = j;
      } else if (h[i] > h[j]) {
        p[j] = i;
      } else {
        p[i] = j;
        h[j]++;
      }
      return true;
    }
    return false;
  }
}
