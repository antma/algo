import std.conv;

pure
auto assignmentProblemMinimize(T) (const T[][] a) {
  enum inf = T.max >> 1;
  immutable int n = a.length.to!int;
  auto G = new T[][n];
  foreach (i; 0 .. n) {
    G[i] = a[i].dup;
  }
  bool[] S, D, E;
  int[] A, B;
  S = new bool[n];
  D = new bool[n];
  E = new bool[n];
  A = new int[n];
  B = new int[n];
  pure nothrow @nogc
  bool rec (int i) {
    if (S[i]) return false;
    S[i] = true;
    foreach (j; 0 .. n) {
      if (!G[i][j] && (B[j] < 0 || rec (B[j]))) {
        A[i] = j;
        B[j] = i;
        return true;
      }
    }
    return false;
  }
  T res;
  while (true) {
    foreach (i; 0 .. n) {
      T min = inf;
      foreach (j; 0 .. n) {
        if (min > G[i][j]) {
          min = G[i][j];
        }
      }
      if (min > 0) {
        res += min;
        foreach (j; 0 .. n) {
          G[i][j] -= min;
        }
      }
    }
    foreach (j; 0 .. n) {
      T min = inf;
      foreach (i; 0 .. n) {
        if (min > G[i][j]) {
          min = G[i][j];
        }
      }
      if (min > 0) {
        res += min;
        foreach (i; 0 .. n) {
          G[i][j] -= min;
        }
      }
    }
    A[] = -1;
    D[] = false;
    B[] = -1;
    E[] = false;
    int c = 0;
    foreach (i; 0 .. n) {
      S[] = false;
      if (rec (i)) {
        D[i] = true;
        ++c;
      } else {
        foreach (j; 0 .. i) {
          if (S[j]) {
            D[j] = false;
            if (A[j] >= 0) {
              E[A[j]] = true;
            }
          }
        }
      }
    }
    if (c == n) break;
    T min_g = inf;
    foreach (i; 0 .. n) if (!D[i]) {
      foreach (j; 0 .. n) if (!E[j]) {
        if (min_g > G[i][j]) {
          min_g = G[i][j];
        }
      }
    }
    res += min_g * (n - c);
    foreach (i; 0 .. n) {
      if (D[i]) {
        G[i][] += min_g;
      }
    }
    foreach (j; 0 .. n) if (!E[j]) {
      foreach (i; 0 .. n) {
        G[i][j] -= min_g;
      }
    }
  }
  return res;
}

pure
auto assignmentProblemMaximize(T) (const T[][] a) {
  auto b = new T[][a.length];
  foreach (i; 0 .. a.length) {
    b[i] = a[i].dup;
    b[i][] *= -1;
  }
  return -assignmentProblemMinimize (b);
}

unittest {
  import std.stdio;
  writeln ("Testing ", __FILE__, " ...");
  assert (assignmentProblemMaximize
    ( [[7,53,183,439,863],
       [497,383,563,79,973],
       [287,63,343,169,583],
       [627,343,773,959,943],
       [767,473,103,699,303]]) == 3315);
}
