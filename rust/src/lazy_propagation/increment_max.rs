//lazy propagation (increment+max)

pub struct LPIncrementMax<T, U, F> {
  n: usize,
  h: u32,
  a: Vec<U>,
  d: Vec<T>,
  combine: F,
}

impl<T, U, F> LPIncrementMax<T, U, F>
where
  for<'b> U: std::ops::AddAssign<&'b T>,
  for<'b> T: std::ops::AddAssign<&'b T>,
  T: Eq + From<i8> + Clone,
  U: Default + From<T>,
  F: Fn(&U, &U) -> U,
{
  fn apply(&mut self, p: usize, value: &T) {
    self.a[p] += value;
    if p < self.n {
      self.d[p] += value;
    }
  }
  fn build(&mut self, mut p: usize) {
    while p > 1 {
      p >>= 1;
      let j = p << 1;
      self.a[p] = (self.combine)(&self.a[j], &self.a[j + 1]);
      self.a[p] += &self.d[p];
    }
  }
  fn push(&mut self, p: usize) {
    let z = T::from(0i8);
    for s in (1..=self.h).rev() {
      let i = p >> s;
      let di = self.d[i].clone();
      if di != z {
        self.d[i] = T::from(0i8);
        let k = i << 1;
        self.apply(k, &di);
        self.apply(k + 1, &di);
      }
    }
  }
  pub fn increment(&mut self, mut l0: usize, mut r0: usize, value: T) {
    l0 += self.n;
    r0 += self.n;
    let mut l = l0;
    let mut r = r0;
    while l < r {
      if (l & 1) != 0 {
        self.apply(l, &value);
        l += 1;
      }
      if (r & 1) != 0 {
        r -= 1;
        self.apply(r, &value);
      }
      l >>= 1;
      r >>= 1;
    }
    self.build(l0);
    self.build(r0 - 1);
  }
  pub fn query(&mut self, start: U, l0: usize, r0: usize) -> U {
    let mut l = l0 + self.n;
    let mut r = r0 + self.n;
    self.push(l);
    self.push(r - 1);
    let mut res = start;
    while l < r {
      if (l & 1) != 0 {
        res = (self.combine)(&res, &self.a[l]);
        l += 1;
      }
      if (r & 1) != 0 {
        r -= 1;
        res = (self.combine)(&self.a[r], &res);
      }
      l >>= 1;
      r >>= 1;
    }
    res
  }
  pub fn new(t: Vec<T>, combine: F) -> Self {
    let n = t.len();
    let h = 8 * std::mem::size_of::<usize>() as u32 - 1 - n.leading_zeros();
    let mut a = Vec::with_capacity(2 * n);
    for _ in 0..n {
      a.push(U::default());
    }
    for x in t {
      a.push(U::from(x));
    }
    let d = vec![T::from(0i8); n];
    for i in (1..n).rev() {
      let k = i << 1;
      a[i] = combine(&a[k], &a[k + 1]);
    }
    Self {
      n,
      h,
      a,
      d,
      combine,
    }
  }
}
