use crate::number_theory::intm::mulm;

pub struct BinomialsM {
  facts: Vec<u32>,
  ifacts: Vec<u32>,
  modulo: u32,
}

impl BinomialsM {
  pub fn new(maxn: usize, modulo: u32) -> Self {
    let mut ifacts = Vec::with_capacity(maxn + 1);
    ifacts.push(1);
    ifacts.push(1);
    for a in 2..=maxn as u32 {
      let q = modulo / a;
      let r = modulo - a * q;
      ifacts.push(mulm(modulo - q, ifacts[r as usize], modulo));
    }
    let mut facts = Vec::with_capacity(maxn + 1);
    facts.push(1);
    facts.push(1);
    let mut t = 1;
    let mut u = 1;
    for a in 2..=maxn {
      t = mulm(t, a as u32, modulo);
      facts.push(t);
      let w = &mut ifacts[a];
      u = mulm(*w, u, modulo);
      *w = u;
    }
    Self {
      facts,
      ifacts,
      modulo,
    }
  }
  pub fn mul(&self, x: u32, y: u32) -> u32 {
    mulm(x, y, self.modulo)
  }
  pub fn binomial(&self, n: u32, k: u32) -> u32 {
    let n = n as usize;
    let k = k as usize;
    let v1 = self.facts[n];
    let v2 = self.mul(self.ifacts[k], self.ifacts[n - k]);
    self.mul(v1, v2)
  }
}
