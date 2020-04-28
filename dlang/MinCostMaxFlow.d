import std.algorithm;
import std.array;
import std.conv;
import std.traits;

final class LevitDeque {
  int[] q;
  immutable int capacity;
  int left, right, elems;
  @property
  bool empty () const { return elems == 0; }
  @property
  int front () const { return q[left]; }
  this (int capacity) {
    this.capacity = capacity;
    q = uninitializedArray!(int[])(capacity);
  }
  void insertFront (int x) {
    assert (elems < capacity);
    if (!elems) {
      q[0] = x;
      left = right = 0;
    } else {
      if (--left < 0) left = capacity - 1;
      q[left] = x;
    }
    ++elems;
  }
  void insertBack (int x) {
    assert (elems < capacity);
    if (!elems) {
      q[0] = x;
      left = right = 0;
    } else {
      if (++right >= capacity) right = 0;
      q[right] = x;
    }
    ++elems;
  }
  void popFront () {
    assert (elems > 0);
    if (++left >= capacity) left = 0;
    --elems;
  }
}

struct MinCostMaxFlowEdge(F, C) {
  F f;
  immutable F capacity;
  immutable C cost;
  immutable int v, e;
  F residualCapacity () const { return capacity - f; }
  bool residualEdge () const { return f < capacity; }
  this (in F _cap, in C _c, in int _v, in int _e) {
    capacity = _cap;
    cost = _c;
    v = _v;
    e = _e;
  }
}

final class MinCostMaxFlowGraph(F, C)
  if (isNumeric!F && isNumeric!C)
{
  alias Edge = MinCostMaxFlowEdge!(F, C);
  private:
  immutable int n;
  Edge[][] edges;
  bool augmentingPath (ref F f, ref C c) {
    auto d = uninitializedArray!(C[])(n);
    d[0] = 0;
    d[1 .. $] = C.max;
    auto pe = uninitializedArray!(int[])(n);
    auto color = uninitializedArray!(byte[])(n);
    color[0] = cast(byte) 1;
    color[1 .. $] = cast(byte) 2;
    auto m1 = new LevitDeque (n);
    m1.insertFront (0);
    while (!m1.empty) {
      const int i = m1.front;
      color[i] = cast(byte) 0;
      m1.popFront ();
      foreach (const ref p; edges[i]) if (p.residualEdge ()) {
        const int j = p.v;
        const w = d[i] + p.cost;
        if (color[j] == 2) {
          color[j] = cast(byte) 1;
          m1.insertBack (j);
          d[j] = w;
          pe[j] = p.e;
        } else if (color[j] == 1) {
          if (d[j] > w) {
            d[j] = w;
            pe[j] = p.e;
          }
        } else {
          assert (color[j] == 0);
          if (d[j] > w) {
            d[j] = w;
            pe[j] = p.e;
            color[j] = cast(byte) 1;
            m1.insertFront (j);
          }
        }
      }
    }
    int i = n - 1;
    if (d[i] == C.max) return false;
    auto delta = F.max;
    while (i > 0) {
      const q = &edges[i][pe[i]];
      const int j = q.v;
      const w = &edges[j][q.e];
      delta = min (delta, w.residualCapacity ());
      i = j;
    }
    i = n - 1;
    while (i > 0) {
      auto q = &edges[i][pe[i]];
      const int j = q.v;
      auto w = &edges[j][q.e];
      w.f += delta;
      q.f -= delta;
      assert (w.f == -q.f);
      i = j;
    }
    f += delta;
    c += delta * d[n - 1];
    return true;
  }
  public:
  void addEdge (int i, int j, F w, C cost) {
    immutable ei = edges[i].length.to!int, ej = edges[j].length.to!int;
    edges[i] ~= Edge (w, cost, j, ej);
    edges[j] ~= Edge (0, -cost, i, ei);
  }
  //0 - sink, (n - 1) - target
  this (int _n) {
    n = _n;
    edges = new Edge[][n];
  }
  void minCostMaxFlow (out F f, out C c) {
    while (augmentingPath (f, c)) {}
  }
}

unittest {
  import std.format;
  auto g = new MinCostMaxFlowGraph!(int, int) (6);
  g.addEdge (0, 1, 1, 1);
  g.addEdge (0, 2, 1, 4);
  g.addEdge (1, 3, 1, 2);
  g.addEdge (3, 4, 1, 3);
  g.addEdge (2, 4, 1, 1);
  g.addEdge (4, 5, 1, 10);
  int f, c;
  g.minCostMaxFlow (f, c);
  assert (f == 1 && c == 15, format!("f = %d, c = %d")(f, c));
}
