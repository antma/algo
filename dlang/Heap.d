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
  pure nothrow @nogc
  void erase (int pos) {
    if (pos <= --size) {
      const int he = h[pos];
      h[pos] = h[size+1];
      g[h[pos]] = pos;
      if (a[he] < a[h[pos]]) {
        heapifyFront (pos);
      } else if (a[h[pos]] < a[he]) {
        heapifyBack (pos);
      }
    }
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
  bool removeKey (int k) {
    assert (k >= 0 && k < n);
    const int pos = g[k];
    if (!pos) return false;
    g[k] = 0;
    erase (pos);
    return true;
  }
  pure nothrow @nogc
  void updateKey (int k, T value) in {
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
  int extractMin () in {
    assert (size > 0);
  } body {
    const he = h[1];
    g[he] = 0;
    erase (1);
    return he;
  }
  pure nothrow @nogc
  inout(T) opIndex (size_t index) inout {
    return a[index];
  }

  @property pure nothrow @nogc
  bool empty () const { return size == 0; }

  this (int _n, T default_value = T.init) {
    n = _n;
    size = 0;
    a = uninitializedArray! (T[]) (n);
    a[] = default_value;
    h = uninitializedArray! (int[]) (n + 1);
    g = new int[n];
  }
}

final class ImplicitKeyHeap(alias less) {
  private:
    int[] h, g;
    int n, size;
  pure nothrow @nogc
  void heapifyFront (const int k) {
    const he = h[k];
    int i = k, j = i << 1;
    while (j <= size) {
      if (j < size && binaryFun!less (h[j+1], h[j])) {
        j++;
      }
      if (!binaryFun!less (h[j], he)) {
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
  void heapifyBack (const int k) {
    const he = h[k];
    int i = k;
    while (i > 1) {
      int j = i >> 1;
      if (!binaryFun!less (he, h[j])) {
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
  void insert (const int i) {
    h[++size] = i;
    g[i] = size;
    heapifyBack (size);
  }
  pure nothrow @nogc
  void erase (const int pos) {
    if (pos <= --size) {
      const he = h[pos];
      h[pos] = h[size+1];
      g[h[pos]] = pos;
      if (binaryFun!less (he, h[pos])) {
        heapifyFront (pos);
      } else if (binaryFun!less (h[pos], he)) {
        heapifyBack (pos);
      }
    }
  }
  public:
  pure nothrow @nogc
  int extractMin () {
    assert (size > 0);
    const int he = h[1];
    g[he] = 0;
    erase (1);
    return he;
  }
  pure nothrow @nogc
  void insertKey (const int key) {
    assert (key >= 0 && key < n);
    assert (g[key] == 0);
    insert (key);
  }
  pure nothrow @nogc
  void removeKey (const int key) {
    assert (key >= 0 && key < n);
    int pos = g[key];
    assert (pos > 0);
    g[key] = 0;
    erase (pos);
  }

  @property pure nothrow @nogc
  bool empty () const { return size == 0; }

  @property pure nothrow @nogc
  int min () const {
    assert (size > 0);
    return h[1];
  }

  this (int _n) {
    n = _n;
    h = uninitializedArray!(int[])(n + 1);
    g = new int[n];
  }
}

unittest {
  import std.stdio, std.conv, std.algorithm;
  writeln ("Testing ", __FILE__, " ...");
  auto h = new Heap!int (5, 239);
  assert (h.empty);
  h.updateKey (0, 10);
  int i = h.extractMin;
  assert (i == 0);
  assert (h.empty);
  h.updateKey (4, 8);
  h.updateKey (1, 7);
  h.updateKey (2, 9);
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
