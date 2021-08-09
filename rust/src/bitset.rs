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
