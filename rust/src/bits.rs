pub struct BitIterator {
  u: u32,
}

impl Iterator for BitIterator {
  type Item = u32;
  fn next(&mut self) -> Option<Self::Item> {
    if self.u == 0 {
      None
    } else {
      let i = self.u.trailing_zeros();
      self.u ^= 1 << i;
      Some(i)
    }
  }
}

pub struct SubmasksIterator {
  s: u32,
  m: u32,
}

impl SubmasksIterator {
  pub fn new(m: u32) -> Self {
    Self { s: m, m }
  }
}

impl Iterator for SubmasksIterator {
  type Item = u32;
  fn next(&mut self) -> Option<Self::Item> {
    if self.s == 0 {
      None
    } else {
      let o = Some(self.s);
      self.s = (self.s - 1) & self.m;
      o
    }
  }
}
