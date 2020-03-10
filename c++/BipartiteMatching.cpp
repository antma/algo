//kuhn
#include <vector>

using namespace std;

class Kuhn {
  private:
  int n, m;
  vector<vector<int> > a;
  vector<int> boys, girls;
  vector<bool> used;
  bool go (int i) {
    if (used[i]) return false;
    used[i] = true;
    for (const int j : a[i]) {
      if (girls[j] < 0 || go (girls[j])) {
        girls[j] = i;
        boys[i] = j;
        return true;
      }
    }
    return false;
  }
  public:
  Kuhn (int _n, int _m) : n (_n), m (_m),
    a (n),
    boys (n), girls (m),
    used (n)
  {}
  void addEdge (int u, int v) {
    a[u].push_back (v);
  }
  int kuhn () {
    int res = 0;
    fill (boys.begin (), boys.end (), -1);
    fill (girls.begin (), girls.end (), -1);
    bool stop = false;
    while (!stop) {
      stop = true;
      fill (used.begin (), used.end (), false);
      for (int i = 0; i < n; ++i) {
        if (boys[i] < 0 && go(i)) {
          ++res;
          stop = false;
        }
      }
    }
    return res;
  }
};
