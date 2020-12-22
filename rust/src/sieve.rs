pub struct Sieve {
  a: Vec<u32>,
}

pub struct Factorization<I>(Vec<(I, u8)>);

impl<I> Factorization<I>
where
  I: std::ops::MulAssign<I> + Ord + From<u32> + Copy,
{
  fn go(&self, v: &mut Vec<I>, c: I, k: usize) {
    if k == self.0.len() {
      v.push(c);
    } else {
      let mut cur = c;
      self.go(v, cur, k + 1);
      for _ in 1..=self.0[k].1 {
        cur *= self.0[k].0;
        self.go(v, cur, k + 1);
      }
    }
  }
  pub fn divisors(&self) -> Vec<I> {
    let mut v = Vec::new();
    self.go(&mut v, I::from(1), 0);
    v.sort();
    v
  }
}

impl Sieve {
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
    Sieve { a }
  }
  pub fn factorization(&self, n: u32) -> Factorization<u32> {
    let mut f = Vec::new();
    let mut x = n as usize;
    let mut last = 0;
    let mut c = 0;
    while x > 1 {
      let p = self.a[x];
      if last != p {
        if last != 0 {
          f.push((last, c));
        }
        c = 1;
        last = p;
      } else {
        c += 1;
      }
      x = ((x as u32) / p) as usize;
    }
    if last != 0 {
      f.push((last, c));
    }
    Factorization(f)
  }
}
