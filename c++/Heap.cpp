#include <cassert>
#include <vector>
#include <numeric>

using namespace std;

template<class T> class Heap {
  private:
    int n;
    int size;
    vector<T> a;
    vector<int> h, g;
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
  void erase (int pos) {
    if (pos <= --size) {
      const int he = h[pos];
      h[pos] = h[size+1];
      g[h[pos]] = pos;
      if (a[he] < h[pos]) {
        heapifyFront (pos);
      } else if (a[h[pos]] < a[he]) {
        heapifyBack (pos);
      }
    }
  }
  public:
  void decreaseKey (int k, T value) {
    assert (k >= 0 && k < n);
    int pos = g[k];
    if (pos == 0) {
      a[k] = value;
      insert (k);
    } else {
      assert (value < a[k]);
      a[k] = value;
      heapifyBack (pos);
    }
  }
  int pollMin () const {
    return h[1];
  }
  int extractMin () {
    assert (size > 0);
    const int he = h[1];
    g[he] = 0;
    erase (1);
    return he;
  }
  bool removeKey (int k) {
    assert (k >= 0 && k < n);
    const int pos = g[k];
    if (pos == 0) return false;
    g[k] = 0;
    erase (pos);
    return true;
  }
  void updateKey (int k, T value) {
    assert (k >= 0 && k < n);
    const int pos = g[k];
    if (pos == 0) {
      a[k] = value;
      insert (k);
    } else if (value < a[k]) {
      a[k] = value;
      heapifyBack (pos);
    } else if (value > a[k]) {
      a[k] = value;
      heapifyFront (pos);
    }
  }

  T operator[] (int index) const {
    return a[index];
  }

  inline bool empty () const { return size == 0; }

  Heap (int _n, T default_value, bool empty = true) :
    n (_n),
    a (n, default_value),
    h (n + 1),
    g (n)
  {
    if (empty) {
      size = 0;
      fill (g.begin (), g.end (), 0);
    } else {
      size = n;
      iota (h.begin () + 1, h.end (), 0);
      iota (g.begin (), g.end (), 1);
    }
  }
};
