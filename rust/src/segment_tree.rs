pub struct SegmentTree<T, F> {
  n: usize,
  f: F,
  t: Vec<T>,
}

impl<T: Copy, F> SegmentTree<T, F>
where
  F: Fn(&T, &T) -> T,
{
  pub fn new(mut a: Vec<T>, f: F) -> Self {
    let n = a.len();
    let mut t = a.clone();
    t.append(&mut a);
    let mut st = SegmentTree { n, f, t };
    st.build();
    st
  }
  fn build(&mut self) {
    for i in (1..self.n).rev() {
      let k = i << 1;
      self.t[i] = (self.f)(&self.t[k], &self.t[k + 1]);
    }
  }
  pub fn update(&mut self, q: usize, v: &T) {
    let mut p = q + self.n;
    self.t[p] = *v;
    while p > 1 {
      self.t[p >> 1] = (self.f)(&self.t[p], &self.t[p ^ 1]);
      p >>= 1;
    }
  }
  pub fn reduce(&self, start: T, u: usize, v: usize) -> T {
    let mut res = start;
    let mut l = u + self.n;
    let mut r = v + self.n;
    while l < r {
      if (l & 1) != 0 {
        res = (self.f)(&res, &self.t[l]);
        l += 1;
      }
      if (r & 1) != 0 {
        r -= 1;
        res = (self.f)(&self.t[r], &res);
      }
      l >>= 1;
      r >>= 1;
    }
    return res;
  }
}
