import std.range;
import std.stdio;
import std.traits;

class InputReader {
  private:
  ubyte[] p;
  ubyte[8<<20] buffer = void;
  size_t cur;
  public:
  final ubyte skipByte (ubyte lo) {
    while (true) {
      auto a = p[cur .. $];
      auto r = a.find! (c => c >= lo);
      if (!r.empty) {
        cur += a.length - r.length;
        return p[cur++];
      }
      p = stdin.rawRead (buffer);
      cur = 0;
      if (p.empty) return 0;
    }
  }
  final ubyte nextByte () {
    if (cur < p.length) {
       return p[cur++];
    }
    p = stdin.rawRead (buffer);
    if (p.empty) return 0;
    cur = 1;
    return p[0];
  }

  template next(T) if (isSigned!T) {
    final T next ()  {
      T res;
      ubyte b = skipByte (45);
      if (b == 45) {
        while (true) {
          b = nextByte ();
          if (b < 48 || b >= 58) {
            return res;
          }
          res = res * 10 - (b - 48);
        }
      } else {
        res = b - 48;
        while (true) {
          b = nextByte ();
          if (b < 48 || b >= 58) {
            return res;
          }
          res = res * 10 + (b - 48);
        }
      }
    }
  }
  template next(T) if (isUnsigned!T) {
    final T next () {
      T res = skipByte (48) - 48;
      while (true) {
        ubyte b = nextByte ();
        if (b < 48 || b >= 58) {
          break;
        }
        res = res * 10 + (b - 48);
      }
      return res;
    }
  }
}
