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
    while a > 0xff && b > 0 {
      let c = a % b;
      a = b;
      b = c;
    }
    if b == 0 {
      a
    } else {
      self.gcd_u8(a, b)
    }
  }
  pub fn gcd_u64(&mut self, x: u64, y: u64) -> u64 {
    let (mut a, mut b) = if x >= y { (x, y) } else { (y, x) };
    while a > 0xffff_ffff && b > 0 {
      let c = a % b;
      a = b;
      b = c;
    }
    if b == 0 {
      a
    } else {
      self.gcd_u32(a as u32, b as u32) as u64
    }
  }
}
