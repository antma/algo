import std.array;

final class Heap(T) {
  private:
    T [] a;
    int [] h;
    int [] g;
    int n;
    int size;
  pure nothrow @nogc
  void heapifyFront (int k) {
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
  pure nothrow @nogc
  void heapifyBack (int k) {
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
  pure nothrow @nogc
  void insert (int i) {
    h[++size] = i;
    g[i] = size;
    heapifyBack (size);
  }
  public:
  pure nothrow @nogc
  void decreaseKey (int k, T value) in {
    assert (k >= 0 && k < n);
    assert (value < a[k]);
  } body {
    a[k] = value;
    immutable pos = g[k];
    if (!pos) {
      insert (k);
    } else {
      heapifyBack (pos);
    }
  }
  pure nothrow @nogc
  void update (int k, T value) in {
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
  pure nothrow @nogc
  int extractMin () {
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
  pure nothrow @nogc
  inout(T) opIndex (size_t index) inout {
    return a[index];
  }

  @property pure nothrow @nogc
  bool empty () const { return size == 0; }

  this (int n_, T default_value = T.init) {
    n = n_;
    size = 0;
    a = uninitializedArray! (T[]) (n);
    a[] = default_value;
    h = new int[n+1];
    g = new int[n];
  }
}

unittest {
  import std.stdio, std.conv, std.algorithm;
  writeln ("Testing ", __FILE__, " ...");
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

  h = new Heap!int (3, 100);
  h.decreaseKey (0, 2);
  h.decreaseKey (1, 3);
  h.decreaseKey (2, 4);
  h.decreaseKey (1, 1);
  assert (h.extractMin () == 1);

  int[] a = [7, 7, 7, 7, 777, 123, 35, 36, 37, 7, 10, 43, 124, 1932, 321, 7, 7, 7, 7, 7, 7, 7];
  h = new Heap!int (a.length.to!int, int.max);
  auto b = a.dup;
  sort (b);
  foreach_reverse (pos, k; a) {
    h.decreaseKey (pos.to!int, k);
  }
  foreach (pos, k; a) {
    immutable j = h.extractMin ();
    assert (b[pos] == h[j]);
  }

  h = new Heap!int (a.length.to!int, int.max);
  foreach_reverse (pos, k; a) {
    h.decreaseKey (pos.to!int, k);
  }
  foreach (pos, k; a) {
    h.decreaseKey (pos.to!int, k / 10);
  }
  foreach (pos, k; a) {
    immutable j = h.extractMin ();
    assert (b[pos] / 10 == h[j]);
  }
}
