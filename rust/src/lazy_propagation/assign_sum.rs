//lazy propagation (assign+sum)
pub struct LPAssignSum<T, S> {
  undef: T,
  n: usize,
  h: u32,
  t: Vec<S>,
  d: Vec<T>,
}

impl<T, S> LPAssignSum<T, S>
where
  T: Copy + PartialEq,
  S: Copy
    + std::convert::From<T>
    + std::convert::From<u32>
    + std::ops::Add<Output = S>
    + std::ops::Mul<Output = S>,
{
  fn calc(&mut self, p: usize, k: usize) {
    if self.d[p] == self.undef {
      self.t[p] = self.t[p << 1] + self.t[(p << 1) + 1];
    } else {
      self.t[p] = S::from(self.d[p]) * S::from(k as u32);
    }
  }
  fn apply(&mut self, p: usize, value: T, k: usize) {
    self.t[p] = S::from(value) * S::from(k as u32);
    if p < self.n {
      self.d[p] = value;
    }
  }
  fn build(&mut self, mut l: usize, mut r: usize) {
    let n = self.n;
    let mut k = 2;
    l += n;
    r += n - 1;
    while l > 1 {
      l >>= 1;
      r >>= 1;
      for i in (l..=r).rev() {
        self.calc(i, k);
      }
      k <<= 1;
    }
  }
  fn push(&mut self, mut l: usize, mut r: usize) {
    let undef = self.undef;
    let n = self.n;
    l += n;
    r += n - 1;
    for s in (1..=self.h).rev() {
      let k = 1 << (s - 1);
      for i in (l >> s)..=(r >> s) {
        let delta = self.d[i];
        if delta != undef {
          self.apply(i << 1, delta, k);
          self.apply((i << 1) + 1, delta, k);
          self.d[i] = undef;
        }
      }
    }
  }
  pub fn assign(&mut self, mut l: usize, mut r: usize, value: T) {
    let n = self.n;
    self.push(l, l + 1);
    self.push(r - 1, r);
    let l0 = l;
    let r0 = r;
    let mut k = 1;
    l += n;
    r += n;
    while l < r {
      if (l & 1) != 0 {
        self.apply(l, value, k);
        l += 1;
      }
      if (r & 1) != 0 {
        r -= 1;
        self.apply(r, value, k);
      }
      l >>= 1;
      r >>= 1;
      k <<= 1;
    }
    self.build(l0, l0 + 1);
    self.build(r0 - 1, r0);
  }
  pub fn sum(&mut self, mut l: usize, mut r: usize) -> S {
    let n = self.n;
    self.push(l, l + 1);
    self.push(r - 1, r);
    let mut res = S::from(0);
    l += n;
    r += n;
    while l < r {
      if (l & 1) != 0 {
        res = res + self.t[l];
        l += 1;
      }
      if (r & 1) != 0 {
        r -= 1;
        res = res + self.t[r];
      }
      l >>= 1;
      r >>= 1;
    }
    res
  }
  pub fn new(a: Vec<T>, undef: T) -> Self {
    let n = a.len();
    let h = 8 * std::mem::size_of::<usize>() as u32 - 1 - n.leading_zeros();
    let mut t = vec![S::from(0); n];
    t.extend(a.into_iter().map(S::from));
    let mut r = Self {
      n,
      h,
      t,
      d: vec![undef; n],
      undef,
    };
    r.build(0, n);
    r
  }
}
