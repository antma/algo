//PrimalityTest32
use crate::number_theory::gcd::Gcd;
use crate::number_theory::intm::mulm;
use crate::number_theory::intm::powm;

//PrimalityTest64
use crate::number_theory::exponentation::generic_pow;
use crate::random::KnuthRandom;

pub struct PrimalityTest32 {
  gcd: Gcd,
}

impl PrimalityTest32 {
  pub fn new() -> Self {
    Self { gcd: Gcd::new() }
  }
  fn witness(a: u32, n: u32) -> bool {
    let n1 = n - 1;
    let m = n1.trailing_zeros();
    let mut x = powm(a, n1 >> m, n);
    for _ in 0..m {
      let y = mulm(x, x, n);
      if y == 1 && x != 1 && x != n1 {
        return true;
      }
      x = y;
    }
    x != 1
  }
  pub fn is_prime(&mut self, n: u32) -> bool {
    if n <= 23 {
      return ((1 << n) & 0x8a28ac) != 0;
    }
    if self.gcd.gcd_u32(n, 223092870) > 1 {
      return false;
    }
    if n <= 529 {
      return true;
    }
    if PrimalityTest32::witness(2, n) || PrimalityTest32::witness(61, n) {
      return false;
    }
    if n < 916327 {
      return true;
    }
    !PrimalityTest32::witness(7, n)
    /*
    if n < 4759123141 {  return true; }
    if PrimalityTest32::witness(3, n) || PrimalityTest32::witness(24251, n) { return false; }
    true
    */
  }
}

struct Montgomery64 {
  n: u64,
  r_mod_n: u64,
  neg_r_mod_n: u64,
  n1: u64,
  rr: u64,
}

fn gcdext_i64(a: i64, b: i64) -> (i64, i64, i64) {
  if b == 0 {
    (a, 1, 0)
  } else {
    let (res, y, x) = gcdext_i64(b, a % b);
    (res, x, y - x * (a / b))
  }
}

const U64_MAX: u64 = 0xffffffffffffffff;

impl Montgomery64 {
  fn new(n: u64) -> Self {
    let d = U64_MAX / n;
    let m = (U64_MAX - d * n) + 1;
    let (r_div_n, r_mod_n) = if m == n { (d + 1, 0) } else { (d, m) };
    let neg_r_mod_n = if r_mod_n != 0 { n - r_mod_n } else { 0 };
    let (_, y, x) = gcdext_i64(n as i64, r_mod_n as i64);
    let y = y - x * r_div_n as i64;
    let n1 = (-y) as u64;
    let m128 = m as u128;
    let rr = ((m128 * m128) % (n as u128)) as u64;
    Self {
      n,
      r_mod_n,
      neg_r_mod_n,
      n1,
      rr,
    }
  }
  fn mul(&self, a: u64, b: u64) -> u64 {
    let a = a as u128;
    let b = b as u128;
    let t = a * b;
    let a = ((t & (U64_MAX as u128)) as u64).wrapping_mul(self.n1);
    let t = t + (a as u128) * (self.n as u128);
    let a = (t >> 64) as u64;
    if a >= self.n {
      a - self.n
    } else {
      a
    }
  }
  fn pow(&self, x: u64, y: u64) -> u64 {
    generic_pow(x, y, |u, v| self.mul(u, v), || self.r_mod_n)
  }
}

pub struct PrimalityTest64 {
  pt32: PrimalityTest32,
  rnd: KnuthRandom,
}

impl PrimalityTest64 {
  pub fn new(seed: i32) -> Self {
    Self {
      pt32: PrimalityTest32::new(),
      rnd: KnuthRandom::new(seed),
    }
  }
  fn witness(m: &Montgomery64, a: u64) -> bool {
    let n1 = m.n - 1;
    let l = n1.trailing_zeros();
    let a = m.mul(a, m.rr);
    let mut x = m.pow(a, n1 >> l);
    for _ in 0..l {
      let y = m.mul(x, x);
      if y == m.r_mod_n && x != m.r_mod_n && x != m.neg_r_mod_n {
        return true;
      }
      x = y;
    }
    x != m.r_mod_n
  }
  pub fn is_prime(&mut self, n: u64, tries: u32) -> bool {
    if n <= 0xffff_ffff {
      return self.pt32.is_prime(n as u32);
    }
    const MODULO: u64 = 223092870;
    let a = n % MODULO;
    if self.pt32.gcd.gcd_u32(MODULO as u32, a as u32) > 1 {
      return false;
    }
    let m = Montgomery64::new(n);
    for a in vec![2, 61, 7] {
      if PrimalityTest64::witness(&m, a) {
        return false;
      }
    }
    if n < 4759123141 {
      return true;
    }
    for _ in 0..tries {
      if PrimalityTest64::witness(&m, self.rnd.randrange(62..0x7fff_ffff) as u64) {
        return false;
      }
    }
    true
  }
}
