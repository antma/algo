use crate::number_theory::factorization::Factorization;

pub struct SieveArray(Vec<u32>);

impl SieveArray {
  pub fn new(n: usize) -> Self {
    let mut a: Vec<_> = (0..n as u32).collect();
    for i in (2..n).step_by(2) {
      a[i] = 2 as u32;
    }
    for p in (3..).step_by(2) {
      if p * p >= n {
        break;
      }
      if a[p] == p as u32 {
        for o in (p * p..n).step_by(2 * p) {
          if a[o] == o as u32 {
            a[o] = p as u32;
          }
        }
      }
    }
    SieveArray(a)
  }
  pub fn factorization(&self, n: u32) -> Factorization {
    let mut f = Vec::new();
    if n == 1 {
      return Factorization(f);
    }
    let mut x = n as usize;
    let mut last = 0;
    let mut c = 0;
    loop {
      let p = self.0[x];
      if last != p {
        if last != 0 {
          f.push((last as u64, c));
        }
        c = 1;
        last = p;
      } else {
        c += 1;
      }
      let t = x as u32;
      if t == p {
        f.push((last as u64, c));
        break Factorization(f);
      }
      x = (t / p) as usize;
    }
  }
}
