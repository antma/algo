use crate::number_theory::gcd::Gcd;
use crate::number_theory::intm::mulm;
use crate::number_theory::intm::powm;

fn witness32(a: u32, n: u32) -> bool {
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

pub fn is_prime32(n: u32, gcd: &mut Gcd) -> bool {
  if n <= 23 {
    return ((1 << n) & 0x8a28ac) != 0;
  }
  if gcd.gcd_u32(n, 223092870) > 1 {
    return false;
  }
  if n <= 529 {
    return true;
  }
  if witness32(2, n) || witness32(61, n) {
    return false;
  }
  if n < 916327 {
    return true;
  }
  !witness32(7, n)
}
