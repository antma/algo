pub fn simpson<F>(a: f64, b: f64, eps: f64, f: F) -> f64
where
  F: Fn(f64) -> f64,
{
  let fa = f(a);
  let fb = f(b);
  let avg = (fa + fb) * 0.5;
  let l = b - a;
  let l3 = l / 3.0;
  let mut n = 2;
  let mut s1 = f((a + b) * 0.5);
  let mut pi = 0.0;
  loop {
    let invn = 1.0 / (n as f64);
    let step = l * invn;
    let mut s2 = 0.0;
    let mut x = a + step * 0.5;
    for _ in 0..n {
      s2 += f(x);
      x += step;
    }
    let i = l3 * invn * (avg + s1 + 2.0 * s2);
    if (i - pi).abs() < eps {
      break i;
    }
    n *= 2;
    s1 += s2;
    pi = i;
  }
}
