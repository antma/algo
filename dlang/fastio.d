import std.range;
import std.stdio;
import std.traits;

class InputReader {
  private:
  ubyte[] p;
  ubyte[] buffer;
  size_t cur;
  public:
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
      while (true) {
        ubyte b = nextByte ();
        if (48 <= b && b < 58) {
          res = b - 48;
          break;
        }
        if (b == 45) {
          while (true) {
            b = nextByte ();
            if (b < 48 || b >= 58) {
              return res;
            }
            res = res * 10 - (b - 48);
          }
        }
      }
      while (true) {
        ubyte b = nextByte ();
        if (b < 48 || b >= 58) {
          return res;
        }
        res = res * 10 + (b - 48);
      }
    }
  }
  template next(T) if (isUnsigned!T) {
    final T next () {
      T res;
      while (true) {
        ubyte b = nextByte ();
        if (48 <= b && b < 58) {
          res = b - 48;
          break;
        }
      }
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
  this () {
    buffer = new ubyte[16<<20];
  }
}

