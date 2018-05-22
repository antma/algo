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
