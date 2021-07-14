use crate::number_theory::gcd::Gcd;
use crate::number_theory::intm::mulm;
use crate::number_theory::intm::powm;

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
    if self.gcd.gcd32(n, 223092870) > 1 {
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
