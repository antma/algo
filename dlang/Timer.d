import std.algorithm;
import std.array;
import std.conv;
import std.math;
import std.range;
import std.stdio;
import std.string;
import std.typecons;

static if (__VERSION__ >= 2075) {
  import std.datetime.stopwatch : StopWatch;
} else {
  import std.datetime : StopWatch;
}

class Timer {
  immutable double timeout;
  StopWatch sw;
  this(double _timeout) {
    timeout = _timeout;
    sw.start ();
  }
  double elapsed () {
    static if (__VERSION__ >= 2075) {
      return sw.peek.total!"usecs" * 1e-6;
    } else {
      return sw.peek.usecs () * 1e-6;
    }
  }
  bool expired () {
    return elapsed () >= timeout;
  }
}
