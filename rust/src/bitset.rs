pub struct BitSet(Vec<u64>);

impl BitSet {
  pub fn clear(&mut self, i: usize) {
    self.0[i >> 6] &= !(1u64 << (i & 63));
  }
  pub fn flip(&mut self, i: usize) {
    self.0[i >> 6] ^= 1u64 << (i & 63);
  }
  pub fn set(&mut self, i: usize) {
    self.0[i >> 6] |= 1u64 << (i & 63);
  }
  pub fn get(&self, i: usize) -> bool {
    if let Some(v) = self.0.get(i >> 6) {
      (v & (1u64 << (i & 63))) != 0
    } else {
      false
    }
  }
  pub fn new(n: usize) -> Self {
    BitSet(vec![0u64; (n + 63) >> 6])
  }
  pub fn set_all(&mut self) {
    for p in &mut self.0 {
      *p = 0xffff_ffff_ffff_ffff;
    }
  }
  pub fn set_range(&mut self, range: std::ops::Range<usize>) {
    let mut u = range.start;
    let mut v = range.end;
    while u < v && (u & 63) != 0 {
      self.set(u);
      u += 1;
    }
    while u < v && (v & 63) != 0 {
      v -= 1;
      self.set(v);
    }
    for p in &mut self.0[(u >> 6)..(v >> 6)] {
      *p = 0xffff_ffff_ffff_ffff;
    }
  }
  pub fn clear_all(&mut self) {
    for p in &mut self.0 {
      *p = 0;
    }
  }
  pub fn bits(&self) -> BitSetIterator {
    BitSetIterator(self, *self.0.first().unwrap_or(&0), 0)
  }
}

pub struct BitSetIterator<'a>(&'a BitSet, u64, usize);

impl<'a> Iterator for BitSetIterator<'a> {
  type Item = usize;
  fn next(&mut self) -> Option<Self::Item> {
    if self.1 == 0 {
      if let Some(k) = (self.2 + 1..(self.0).0.len()).find(|j| (self.0).0[*j] != 0) {
        self.2 = k;
        self.1 = (self.0).0[k];
      } else {
        return None;
      }
    }
    let i = self.1.trailing_zeros();
    self.1 ^= 1u64 << i;
    Some((self.2 << 6) + i as usize)
  }
}

impl std::ops::BitOrAssign<&BitSet> for BitSet {
  fn bitor_assign(&mut self, rhs: &BitSet) {
    if self.0.len() < rhs.0.len() {
      self.0.resize(rhs.0.len(), 0u64);
    }
    for (u, v) in self.0.iter_mut().zip(rhs.0.iter()) {
      *u |= *v;
    }
  }
}
