import std.array;
import std.functional;
import std.traits;

class SegmentTree(T = int, alias op="a+b", T zero = T.init) {
  private:
  T [] t;
  size_t n;
  final void build () pure nothrow @nogc {
    foreach_reverse (i; 1 .. n) {
      immutable k = i << 1;
      t[i] = binaryFun!op (t[k], t[k+1]);
    }
  }
  public:
  final void update (size_t p, T v) pure nothrow @nogc {
    for (t[p += n] = v; p > 1; p >>= 1) {
      t[p>>1] = binaryFun!op (t[p], t[p ^ 1]);
    }
  }
  final T reduce (size_t l, size_t r) const pure nothrow @nogc {
    T res = zero;
    for (l += n, r += n; l < r; l >>= 1, r >>= 1) {
      if (l & 1) {
        res = binaryFun!op (res, t[l++]);
      }
      if (r & 1) {
        res = binaryFun!op (t[--r], res);
      }
    }
    return res;
  }
  this (const T[] a) pure nothrow {
    n = a.length;
    t = uninitializedArray!(T[])(n) ~ a;
    build ();
  }
}

class NonCommutativeSegmentTree(T = int, alias op) {
  private:
  T [] t;
  size_t n;
  final void build () {
    foreach_reverse (i; 1 .. n) {
      immutable k = i << 1;
      t[i] = binaryFun!op (t[k], t[k+1]);
    }
  }
  public:
  final void update (size_t p, const T v) {
    for (t[p += n] = v; p > 1; ) {
      p >>= 1;
      immutable k = p << 1;
      t[p] = binaryFun!op (t[k], t[k+1]);
    }
  }
  final T reduce (size_t l, size_t r) const {
    T res1, res2;
    for (l += n, r += n; l < r; l >>= 1, r >>= 1) {
      if (l & 1) {
        res1 = binaryFun!op (res1, t[l++]);
      }
      if (r & 1) {
        res2 = binaryFun!op (t[--r], res2);
      }
    }
    return binaryFun!op (res1, res2);
  }
  this (const T[] a) {
    n = a.length;
    t = new T[n];
    t ~= a;
    build ();
  }
}

//Modification on semi-interval, single element access
class SegmentTreeSliceUpdate(T = int) {
  private:
  T [] t;
  size_t n;
  final void push () pure nothrow @nogc {
    foreach (i; 1 .. n) {
      immutable k = i << 1;
      t[k] = t[k] + t[i];
      t[k+1] = t[k+1] + t[i];
      t[i] = 0;
    }
  }
  final void update (size_t l, size_t r, T v) pure nothrow @nogc {
    for (l += n, r += n; l < r; l >>= 1, r >>= 1) {
      if (l & 1) {
        t[l++] += v;
      }
      if (r & 1) {
        t[--r] += v;
      }
    }
  }
  public:
  final inout(T) opIndex (size_t index) inout pure nothrow @nogc {
     T res;
     for (index += n; index > 0; index >>= 1) {
       res += t[index];
     }
     return res;
  }
  final const(T[]) force () pure nothrow {
    push ();
    return t[n .. 2 * n].idup;
  }
  this (int _n) pure nothrow {
    n = _n;
    t = new T[2 * n];
  }
}

class SetSegmentTree(S, alias combine) if (isSomeFunction!combine) {
  private:
  S [] t;
  size_t n;
  final void build () {
    foreach_reverse (i; 1 .. n) {
      immutable k = i << 1;
      t[i] = combine (t[k], t[k+1]);
    }
  }
  public:
  final U reduce(U, U zero) (size_t l, size_t r, U delegate(U, S) op) {
    U res = zero;
    for (l += n, r += n; l < r; l >>= 1, r >>= 1) {
      if (l & 1) {
        res = op (res, t[l++]);
      }
      if (r & 1) {
        res = op (res, t[--r]);
      }
    }
    return res;
  }
  this (S[] a) {
    n = a.length;
    t = uninitializedArray!(S[])(n) ~ a;
    build ();
  }
}

unittest {
  import std.stdio, std.conv, std.random, std.range;
  writeln ("Testing ", __FILE__, " ...");
  auto st = new SegmentTree!(long, "a+b") ([1L, 2L]);
  assert (st.reduce (0, 2) == 3L);
}
