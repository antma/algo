import std.algorithm;
import std.math;
import std.complex;

alias Real = real;
alias CReal = Complex!real;

class FFT {
  private size_t[] tbl;
  private size_t n, h, ldn;
  
  private final size_t revbinUpdate (const size_t k) {
    if (tbl[k] != size_t.max) return tbl[k];
    size_t r = k, i = h;
    while (! ((r ^= i) & i)) {
      i >>>= 1;
    }
    return tbl[k] = r;
  }
  private final void revbinPermute (CReal[] a) {
    if (n <= 2) return;
    size_t r = 0, x = 1;
    while (x < h) {
      r += h;
      swap (a[x], a[r]);
      ++x;
      r = revbinUpdate (r);
      if (r > x) {
        swap (a[x], a[r]);
        swap (a[n - 1 - x], a[n - 1 - r]);
      }
      ++x;
    }
  }
  
  final fft (CReal[] a, int s) {
    if (ldn < 1) return;
    revbinPermute (a);
    int r = 0;
    while (r < n) {
      immutable u = a[r];
      immutable v = a[r + 1];
      a[r] = u + v;
      a[r+1] = u - v;
      r += 2;
    }
    foreach (t; 2 .. ldn + 1) {
      immutable m = 1 << t;
      immutable mh = m >> 1;
      immutable Real delta = s * 2.0 * PI / m;
      immutable CReal w = std.complex.expi (delta);
      CReal e = CReal (1.0, 0.0);
      foreach (j; 0 .. mh) {
        r = 0;
        while (r < n) {
          CReal *x = &a[r + j];
          CReal *y = x + mh;
          immutable u = *x, v = (*y) * e;
          *x = u + v;
          *y = u - v;
          r += m;
        }
        e *= w;
      }
    }
  }
  
  final normalize (CReal[] a) {
    immutable Real x = 1.0 / n;
    foreach (ref y; a) {
      y *= x; 
    }
  }
  
  final conv (CReal[] c, CReal[] a, CReal[] b) in {
    assert (a.length == n);
    assert (b.length == n);
    assert (c.length == n);
  }
  body {
    fft (a, 1);
    fft (b, 1);
    foreach (i; 0 .. n) {
      c[i] = a[i] * b[i];
    }
    fft (c, -1);
    normalize (c);
  }
  
  this (size_t _n) in {
    assert (0 == (_n & (_n - 1)));
  } body {
    n = _n;
    h = n >>> 1;
    ldn = 0;
    while ((1 << ldn) != n) ++ldn;
    tbl = new size_t[n];
    tbl[] = size_t.max;
  }
}
