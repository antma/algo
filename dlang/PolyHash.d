import std.array;
import std.conv;
import std.random;
import core.checkedint;

struct HashT {
  enum p1 = 4294967291U;
  enum p2 = 4294967279U;
  uint r1, r2;
  static uint addm (const uint x, const uint y, const uint m) {
    bool o;
    immutable uint r = addu (x, y, o);
    return (o || r >= m) ? (r - m) : r;
  }
  static uint subm (const uint x, const uint y, const uint m) {
    bool o;
    immutable uint r = subu (x, y, o);
    return o ? (r + m) : r;
  }
  HashT opBinary (string op : "+")(in HashT rhs) const {
    return HashT (addm (r1, rhs.r1, p1), addm (r2, rhs.r2, p2));
  }
  HashT opBinary (string op : "-")(in HashT rhs) const {
    return HashT (subm (r1, rhs.r1, p1), subm (r2, rhs.r2, p2));
  }
  HashT opBinary (string op : "*")(in HashT rhs) const {
    return HashT (((r1.to!ulong * rhs.r1) % p1).to!uint, ((r2.to!ulong * rhs.r2) % p2).to!uint);
  }
  ulong get () const {
    return (r1.to!ulong << 32) + r2;
  }
}

final class PolyHash {
  private:
  HashT[] h, d;
  public:
  this (in string s) {
    immutable n = s.length.to!int;
    auto p = HashT (uniform (256, int.max), uniform (256, int.max));
    h = uninitializedArray!(HashT[]) (n + 1);
    d = uninitializedArray!(HashT[]) (n + 1);
    h[0] = HashT (0, 0);
    d[0] = HashT (1, 1);
    foreach (i; 0 .. n) {
      uint c = s[i].to!uint;
      h[i+1] = h[i] * p + HashT (c, c);
      d[i+1] = d[i] * p;
    }
  }
  //[l, r)
  ulong get (int l, int r) const {
    return (h[r] - h[l] * d[r - l]).get ();
  }
}
