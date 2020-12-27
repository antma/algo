#[test]
fn totients() {
  let phi = algo::linear_sieve::totients(1001);
  assert_eq!(phi.len(), 1001);
  assert_eq!(
    phi
      .into_iter()
      .enumerate()
      .skip(1)
      .map(|(i, x)| i as i32 - x)
      .sum::<i32>(),
    196308
  );
}

#[test]
fn mus() {
  let m2 = vec![
    1, -1, -1, 0, -1, 1, -1, 0, 0, 1, -1, 0, -1, 1, 1, 0, -1, 0, -1, 0, 1, 1, -1, 0, 0, 1, 0, 0,
    -1, -1, -1, 0, 1, 1, 1, 0, -1, 1, 1, 0, -1, -1, -1, 0, 0, 1, -1, 0, 0, 0, 1, 0, -1, 0, 1, 0, 1,
    1, -1, 0, -1, 1, 0, 0, 1, -1, -1, 0, 1, -1, -1, 0, -1, 1, 0, 0, 1, -1,
  ];
  let m1: Vec<_> = algo::linear_sieve::mus(79).into_iter().skip(1).collect();
  assert_eq!(m1, m2);
}

fn harm_slow(n: u32, f: fn(u32) -> u32) -> u32 {
  (1..=n).map(|i| f(n / i)).sum()
}

fn harm_fast(n: u32, f: fn(u32) -> u32) -> u32 {
  let h = algo::linear_sieve::HarmonicSeries::new(n);
  let mut s = 0;
  for (r, p) in h {
    s += (r.end - r.start) * f(p);
  }
  s
}

fn harm_sqrt_slow(n: u32, f: fn(u32) -> u32) -> u32 {
  (1..=n).take_while(|i| i * i <= n).map(|i| f(n / i)).sum()
}

fn harm_sqrt_fast(n: u32, f: fn(u32) -> u32) -> u32 {
  let h = algo::linear_sieve::HarmonicSeries::new(n);
  let i = (n as f64).sqrt().floor() as u32;
  let mut s = 0;
  for (r, p) in h {
    let v = r.end.min(i + 1);
    if r.start >= v {
      break;
    }
    s += (v - r.start) * f(p);
  }
  s
}

#[test]
fn harmonic() {
  for n in 1..=50 {
    assert_eq!(harm_slow(n, |x| x), harm_fast(n, |x| x));
  }
  for n in 1..=50 {
    assert_eq!(harm_slow(n, |x| x * x), harm_fast(n, |x| x * x));
  }
  for n in 1..=250 {
    assert_eq!(harm_sqrt_slow(n, |x| x * x), harm_sqrt_fast(n, |x| x * x));
  }
}
