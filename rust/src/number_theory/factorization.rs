use crate::number_theory::gcd::Gcd;
use crate::number_theory::primality_test64::is_prime64;
use crate::random::KnuthRandom;

pub struct Factorization(pub Vec<(u64, u8)>);

fn pollard_rho(n: u64, gcd: &mut Gcd, rnd: &mut KnuthRandom) -> Option<u64> {
  let x0 = rnd.randrange(0..n.min(0x7fff_ffff) as u32) as u64;
  let mut y = x0;
  let mut x = x0;
  let mut k = 2;
  let its = (n as f64).sqrt().sqrt().round() as u32 + 10;
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
      k = 2 * k;
    }
  }
  None
}

fn pollard_factorization(
  p: u64,
  gcd: &mut Gcd,
  rnd: &mut KnuthRandom,
  tries: u32,
) -> std::collections::HashMap<u64, u8> {
  let mut h = std::collections::HashMap::new();
  if p == 1 {
    return h;
  }
  if is_prime64(p, gcd, rnd, tries) {
    h.insert(p, 1);
    return h;
  }
  loop {
    if let Some(d) = pollard_rho(p, gcd, rnd) {
      let d = d.min(p / d);
      let w = pollard_factorization(d, gcd, rnd, tries);
      let mut u = p / d;
      for q in w.iter() {
        while u % q.0 == 0 {
          let e = h.entry(*q.0).or_insert(0);
          *e += 1;
          u = u / q.0;
        }
      }
      for q in w {
        let e = h.entry(q.0).or_insert(0);
        *e += q.1;
      }
      for q in pollard_factorization(u, gcd, rnd, tries) {
        let e = h.entry(q.0).or_insert(0);
        *e += q.1;
      }
      break h;
    }
  }
}

pub fn factorization64(
  p: u64,
  small_primes: &Vec<u32>,
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
      p = p / q;
      while p % (q as u64) == 0 {
        t += 1;
        p = p / q;
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
  r.extend(pollard_factorization(p, gcd, rnd, tries));
  r.sort();
  Factorization(r)
}
