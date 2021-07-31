use crate::math::sqrtint;
use crate::number_theory::exponentation::generic_pow;
use crate::number_theory::gcd::Gcd;
use crate::number_theory::primality_test64::is_prime64;
use crate::random::KnuthRandom;
use std::collections::HashMap;

pub struct Factorization(pub Vec<(u64, u8)>);

impl Factorization {
  fn go(&self, v: &mut Vec<u64>, c: u64, k: usize) {
    if k == self.0.len() {
      v.push(c);
    } else {
      let mut cur = c;
      self.go(v, cur, k + 1);
      for _ in 1..=self.0[k].1 {
        cur *= self.0[k].0;
        self.go(v, cur, k + 1);
      }
    }
  }
  pub fn divisors(&self) -> Vec<u64> {
    let mut v = Vec::new();
    self.go(&mut v, 1, 0);
    v.sort();
    v
  }
}

fn pollard_monte_carlo(
  n: u64,
  gcd: &mut Gcd,
  small_primes: &[u32],
  rnd: &mut KnuthRandom,
) -> Option<u64> {
  let n128 = n as u128;
  let mut b = rnd.randrange(2..*small_primes.last().unwrap() + 1) as u64;
  for q in small_primes {
    let mut cur = *q as u64;
    while let Some(w) = cur.checked_mul(*q as u64) {
      if w > n {
        break;
      }
      cur = w;
      b = generic_pow(
        b,
        cur,
        |x, y| (((x as u128) * (y as u128)) % n128) as u64,
        || 1u64,
      );
      if b > 0 {
        let g = gcd.gcd_u64(n, b - 1);
        if g > 1 && g < n {
          return Some(g);
        }
      }
    }
  }
  None
}

fn pollard_rho(n: u64, gcd: &mut Gcd, rnd: &mut KnuthRandom) -> Option<u64> {
  let x0 = rnd.randrange(0..n.min(0x7fff_ffff) as u32) as u64;
  let mut y = x0;
  let mut x = x0;
  let mut k = 2;
  let its = 3 * (n as f64).sqrt().sqrt().round() as u32 + 10;
  for i in 2..its {
    let xi = if x != 0 {
      let z = x as u128;
      ((z * z - 1) % (n as u128)) as u64
    } else {
      n - 1
    };
    let d = gcd.gcd_u64(n, if y >= xi { y - xi } else { xi - y });
    if d != 1 && d != n {
      return Some(d);
    }
    x = xi;
    if i == k {
      y = xi;
      k <<= 1;
    }
  }
  None
}

fn ferma(n: u64) -> u64 {
  let mut x = sqrtint(n) as u64;
  let mut y = 0;
  let mut xx = x * x;
  let mut yy = n;
  loop {
    if xx == yy {
      break if x != y { x - y } else { x + y };
    }
    if xx > yy {
      yy += y + y + 1;
      y += 1;
    } else {
      xx += x + x + 1;
      x += 1;
    }
  }
}

fn factorization_large(
  p: u64,
  small_primes: &[u32],
  gcd: &mut Gcd,
  rnd: &mut KnuthRandom,
  tries: u32,
  h: &mut HashMap<u64, u8>,
) {
  if p == 1 {
    return;
  }
  if is_prime64(p, gcd, rnd, tries) {
    *(h.entry(p).or_insert(0)) += 1;
    return;
  }
  let d = loop {
    let d = pollard_monte_carlo(p, gcd, small_primes, rnd);
    if d.is_some() {
      break d.unwrap();
    }
    let d = pollard_rho(p, gcd, rnd);
    if d.is_some() {
      break d.unwrap();
    }
    break ferma(p);
  };
  factorization_large(d, small_primes, gcd, rnd, tries, h);
  factorization_large(p / d, small_primes, gcd, rnd, tries, h);
}

pub fn factorization64(
  p: u64,
  small_primes: &[u32],
  gcd: &mut Gcd,
  rnd: &mut KnuthRandom,
  tries: u32,
) -> Factorization {
  let mut r = Vec::new();
  if p == 1 {
    return Factorization(r);
  }
  if is_prime64(p, gcd, rnd, tries) {
    r.push((p, 1));
    return Factorization(r);
  }
  let mut p = p;
  //trial division
  for q in small_primes.iter().map(|w| *w as u64) {
    if q * q > p {
      r.push((p, 1));
      return Factorization(r);
    }
    if p % (q as u64) == 0 {
      let mut t = 1;
      p /= q;
      while p % (q as u64) == 0 {
        t += 1;
        p /= q;
      }
      r.push((q as u64, t));
      if p == 1 {
        return Factorization(r);
      }
      if is_prime64(p, gcd, rnd, tries) {
        r.push((p, 1));
        return Factorization(r);
      }
    }
  }
  let mut h = HashMap::new();
  let sp = &small_primes[0..small_primes.len().min(25)];
  factorization_large(p, sp, gcd, rnd, tries, &mut h);
  r.extend(h);
  r.sort();
  Factorization(r)
}
