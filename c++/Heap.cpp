#include <cassert>
#include <vector>

using namespace std;

template<class T> class Heap {
  private:
    vector<T> a;
    vector<int> h, g;
    int n;
    int size;
  void heapifyFront (int k) {
    const int he = h[k];
    int i = k, j = i << 1;
    while (j <= size) {
      if (j < size && a[h[j+1]] < a[h[j]]) {
        j++;
      }
      if (a[h[j]] >= a[he]) {
        break;
      }
      h[i] = h[j];
      g[h[i]] = i;
      i = j;
      j = i << 1;
    }
    if (i != k) {
      h[i] = he;
      g[he] = i;
    }
  }
  void heapifyBack (int k) {
    const int he = h[k];
    int i = k;
    while (i > 1) {
      int j = i >> 1;
      if (a[he] >= a[h[j]]) {
        break;
      }
      h[i] = h[j];
      g[h[i]] = i;
      i = j;
    }
    if (i != k) {
      h[i] = he;
      g[he] = i;
    }
  }
  void insert (int i) {
    h[++size] = i;
    g[i] = size;
    heapifyBack (size);
  }
  public:
  void decreaseKey (int k, T value) {
    assert (k >= 0 && k < n);
    int pos = g[k];
    if (pos < 0) {
      a[k] = value;
      insert (k);
    } else {
      assert (value < a[k]);
      a[k] = value;
      heapifyBack (pos);
    }
  }
  int extractMin () {
    assert (size > 0);
    const int he = h[1];
    g[he] = -1;
    h[1] = h[size--];
    g[h[1]] = 1;
    if (size) {
      heapifyFront (1);
    }
    return he;
  }

  T operator[] (int index) const {
    return a[index];
  }

  inline bool empty () const { return size == 0; }

  Heap (int n_, T default_value) {
    n = n_;
    size = 0;
    a.assign (n, default_value);
    h.assign (n + 1, 0);
    g.assign (n, -1);
  }
};
