use crate::number_theory::intm::{addm, mulm, subm};
use std::ops::{AddAssign, MulAssign, SubAssign};

const P1: u32 = 4294967291;
const P2: u32 = 4294967279;

#[derive(Clone)]
struct HashT {
  r1: u32,
  r2: u32,
}

impl AddAssign<&HashT> for HashT {
  fn add_assign(&mut self, rhs: &HashT) {
    self.r1 = addm(self.r1, rhs.r1, P1);
    self.r2 = addm(self.r2, rhs.r2, P2);
  }
}

impl SubAssign<&HashT> for HashT {
  fn sub_assign(&mut self, rhs: &HashT) {
    self.r1 = subm(self.r1, rhs.r1, P1);
    self.r2 = subm(self.r2, rhs.r2, P2);
  }
}

impl MulAssign<&HashT> for HashT {
  fn mul_assign(&mut self, rhs: &HashT) {
    self.r1 = mulm(self.r1, rhs.r1, P1);
    self.r2 = mulm(self.r2, rhs.r2, P2);
  }
}

impl HashT {
  fn get(&self) -> u64 {
    ((self.r1 as u64) << 32) + (self.r2 as u64)
  }
  fn new(r: u32) -> Self {
    HashT { r1: r, r2: r }
  }
}

pub struct PolyHash {
  pub n: usize,
  h: Vec<HashT>,
  d: Vec<HashT>,
}

impl PolyHash {
  pub fn new(s: &str, b: &(u32, u32)) -> Self {
    let p = HashT { r1: b.0, r2: b.1 };
    let b = s.as_bytes();
    let n = b.len();
    let mut h = Vec::<HashT>::with_capacity(n);
    let mut d = Vec::<HashT>::with_capacity(n);
    h.push(HashT::new(0));
    d.push(HashT::new(1));
    for i in 0..n {
      let c = b[i] as u32;
      let x = HashT { r1: c, r2: c };
      let mut u = h[i].clone();
      u *= &p;
      u += &x;
      h.push(u);
      u = d[i].clone();
      u *= &p;
      d.push(u);
    }
    PolyHash { n, h, d }
  }
  pub fn get(&self, l: usize, r: usize) -> u64 {
    let mut v = self.h[l].clone();
    v *= &self.d[r - l];
    let mut u = self.h[r].clone();
    u -= &v;
    u.get()
  }
}
