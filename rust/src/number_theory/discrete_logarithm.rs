//discrete logarithm, baby step giant step
use crate::number_theory::intm;
use intm::mulm;
use intm::powm;

pub struct DiscreteLog {
  n: u32,
  a: u32,
  modulo: u32,
  h: std::collections::HashMap<u32, u32>,
}

impl DiscreteLog {
  pub fn new(a: u32, modulo: u32) -> Self {
    let n = (modulo as f64).sqrt().ceil() as u32;
    let mut h = std::collections::HashMap::new();
    let an = powm(a, n, modulo);
    let mut cur = an;
    for p in 1..=n {
      h.entry(cur).or_insert(p);
      cur = mulm(cur, an, modulo);
    }
    Self { n, a, modulo, h }
  }
  //a ^ res == x (mod modulo), modulo should be prime
  pub fn discrete_log(&self, x: u32) -> Option<u32> {
    let a = self.a;
    let mut cur = x;
    let mut res = self.modulo;
    for q in 0..=self.n {
      if let Some(p) = self.h.get(&cur) {
        let w = *p * self.n - q;
        if res > w {
          res = w;
        }
      }
      cur = mulm(cur, a, self.modulo);
    }
    if res == self.modulo {
      None
    } else {
      Some(res)
    }
  }
}
