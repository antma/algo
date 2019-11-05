import std.algorithm;
import std.array;
import std.range;
import std.stdio;
import std.traits;

final class InputReader {
  private:
  ubyte[] p, buffer;
  bool eof;
  bool rawRead () {
    if (eof) {
      return false;
    }
    p = stdin.rawRead (buffer);
    if (p.empty) {
      eof = true;
      return false;
    }
    return true;
  }
  ubyte nextByte(bool check) () {
    static if (check) {
      if (p.empty) {
        if (!rawRead ()) {
          return 0;
        }
      }
    }
    auto r = p.front;
    p = dropOne (p);
    return r;
  }
  public:
  this () {
    buffer = uninitializedArray!(ubyte[])(16<<20);
  }
  bool seekByte (in ubyte lo) {
    while (true) {
      p = p.find! (c => c >= lo);
      if (!p.empty) {
        return false;
      }
      if (!rawRead ()) {
        return true;
      }
    }
  }
  template next(T) if (isSigned!T) {
    T next ()  {
      if (seekByte (45)) {
        return 0;
      }
      T res;
      ubyte b = nextByte!false ();
      if (b == 45) {
        while (true) {
          b = nextByte!true ();
          if (b < 48 || b >= 58) {
            return res;
          }
          res = res * 10 - (b - 48);
        }
      } else {
        res = b - 48;
        while (true) {
          b = nextByte!true ();
          if (b < 48 || b >= 58) {
            return res;
          }
          res = res * 10 + (b - 48);
        }
      }
    }
  }
  template next(T) if (isUnsigned!T) {
    T next () {
      if (seekByte (48)) {
        return 0;
      }
      T res = nextByte!false () - 48;
      while (true) {
        ubyte b = nextByte!true ();
        if (b < 48 || b >= 58) {
          break;
        }
        res = res * 10 + (b - 48);
      }
      return res;
    }
  }
  T[] nextA(T) (in int n) {
    auto a = uninitializedArray!(T[]) (n);
    foreach (i; 0 .. n) {
      a[i] = next!T;
    }
    return a;
  }
}
