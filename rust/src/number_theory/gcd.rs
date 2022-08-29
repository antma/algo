pub struct Gcd {
  dp: Vec<Option<std::num::NonZeroU8>>,
}

impl Gcd {
  pub fn new() -> Self {
    Self {
      dp: vec![None; 32896],
    }
  }
  fn gcd_u8(&mut self, x: u32, y: u32) -> u32 {
    let i = 0x100 - y;
    let idx = (((i * (i - 1)) >> 1) + (x - y)) as usize;
    if let Some(t) = &self.dp[idx] {
      return t.get() as _;
    }
    let res = if y == 0 { x } else { self.gcd_u8(y, x % y) };
    self.dp[idx] = std::num::NonZeroU8::new(res as u8);
    res
  }
  pub fn gcd_u32(&mut self, x: u32, y: u32) -> u32 {
    let (mut a, mut b) = if x >= y { (x, y) } else { (y, x) };
    if b == 0 {
      return a;
    }
    let la = a.trailing_zeros();
    let lb = b.trailing_zeros();
    let l = la.min(lb);
    a >>= la;
    b >>= lb;
    if a < b {
      std::mem::swap(&mut a, &mut b);
    }
    while a > 0xff {
      if a == b {
        return a << l;
      }
      a -= b;
      a >>= a.trailing_zeros();
      if a < b {
        std::mem::swap(&mut a, &mut b);
      }
    }
    (self.gcd_u8(a, b) as u32) << l
  }
  pub fn gcd_u64(&mut self, x: u64, y: u64) -> u64 {
    let (mut a, mut b) = if x >= y { (x, y) } else { (y, x) };
    if b == 0 {
      return a;
    }
    let la = a.trailing_zeros();
    let lb = b.trailing_zeros();
    let l = la.min(lb);
    a >>= la;
    b >>= lb;
    if a < b {
      std::mem::swap(&mut a, &mut b);
    }
    while a > 0xff {
      if a == b {
        return a << l;
      }
      a -= b;
      a >>= a.trailing_zeros();
      if a < b {
        std::mem::swap(&mut a, &mut b);
      }
    }
    (self.gcd_u8(a as u32, b as u32) as u64) << l
  }
  pub fn lcm_u64(&mut self, x: u64, y: u64) -> u64 {
    (x / self.gcd_u64(x, y)) * y
  }
}
