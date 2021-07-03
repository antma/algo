pub struct BitIterator(u32);

impl Iterator for BitIterator {
  type Item = u32;
  fn next(&mut self) -> Option<Self::Item> {
    if self.0 == 0 {
      None
    } else {
      let i = self.0.trailing_zeros();
      self.0 ^= 1 << i;
      Some(i)
    }
  }
}

pub struct Bits(u32);
impl IntoIterator for Bits {
  type Item = u32;
  type IntoIter = BitIterator;
  fn into_iter(self) -> Self::IntoIter {
    BitIterator(self.0)
  }
}

pub struct SubmasksIterator {
  s: u32,
  m: u32,
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

//the submask equal to zero will not be processed
pub struct Submasks(pub u32);
impl IntoIterator for Submasks {
  type Item = u32;
  type IntoIter = SubmasksIterator;
  fn into_iter(self) -> Self::IntoIter {
    SubmasksIterator {
      s: self.0,
      m: self.0,
    }
  }
}
