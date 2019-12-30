#include <vector>
#include <numeric>
using namespace std;

class DisjointSet {
  private:
    vector<int> p, h;
    int n;
  public:
  DisjointSet (int _n) :
    h (_n, 0),
    n (_n)
  {
    p.resize (n);
    iota (p.begin (), p.end (), 0);
  }
  int findSet (int x) {
    if (p[x] == x) {
      return x;
    }
    return p[x] = findSet (p[x]);
  }
  bool merge (int i, int j) {
    i = findSet (i);
    j = findSet (j);
    if (i != j) {
      if (h[i] < h[j]) {
        p[i] = j;
      } else if (h[i] > h[j]) {
        p[j] = i;
      } else {
        p[i] = j;
        ++h[j];
      }
      return true;
    }
    return false;
  }
};
