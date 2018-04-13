import std.stdio;

class Heap(T) {
  private:
    T [] a;
    int [] h;
    int [] g;
    int n;
    int size;
  final void heapifyFront (int k) pure nothrow @nogc {
    immutable he = h[k];
    int i = k;
    int j = i << 1;
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
  final void heapifyBack (int k) pure nothrow @nogc {
    immutable he = h[k];
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
  final void insert (int i) pure nothrow @nogc {
    h[++size] = i;
    g[i] = size;
    heapifyBack (size);
  }
  public:
  final void decreaseKey (int k, T value) pure nothrow @nogc in {
    assert (k >= 0 && k < n);
    assert (value < a[k]);
  } body {
    a[k] = value;
    immutable pos = g[k];
    if (!pos) {
      insert (k);
    } else if (value < a[k]) {
      heapifyBack (pos);
    }
  }
  final void update (int k, T value) pure nothrow @nogc in {
    assert (k >= 0 && k < n);
  } body {
    immutable pos = g[k];
    if (!pos) {
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
  final int extractMin () pure nothrow @nogc {
    assert (size > 0);
    immutable he = h[1];
    g[he] = 0;
    if (--size) {
      h[1] = h[size+1];
      g[h[1]] = 1;
      heapifyFront (1);
    }
    return he;
  }

  inout(T) opIndex (size_t index) pure nothrow @nogc inout {
    return a[index];
  }

  @property
  bool empty () const pure nothrow @nogc { return size == 0; }

  this (int n_, T default_value) {
    n = n_;
    size = 0;
    a = new T[n];
    a[] = default_value;
    h = new int[n+1];
    g = new int[n];
  }
}

unittest {
  writeln ("Testing heap.d ...");
  auto h = new Heap!int (5, 239);
  assert (h.empty);
  h.update (0, 10);
  int i = h.extractMin;
  assert (i == 0);
  assert (h.empty);
  h.update (4, 8);
  h.update (1, 7);
  h.update (2, 9);
  i = h.extractMin;
  assert (i == 1);
  i = h.extractMin;
  assert (i == 4);
}
