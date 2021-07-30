pub struct SegmentTree<T, F> {
  n: usize,
  f: F,
  t: Vec<T>,
}

impl<T: Clone, F: Fn(&T, &T) -> T> SegmentTree<T, F> {
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
  pub fn update(&mut self, q: usize, v: T) {
    let mut p = q + self.n;
    self.t[p] = v;
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
    res
  }
}

//modification on range, query in index
impl<T: Clone, F: Fn(&T, &T) -> T> SegmentTree<T, F> {
  pub fn update_range(&mut self, u: usize, v: usize, value: T) {
    let mut l = u + self.n;
    let mut r = v + self.n;
    while l < r {
      if (l & 1) != 0 {
        self.t[l] = (self.f)(&self.t[l], &value);
        l += 1;
      }
      if (r & 1) != 0 {
        r -= 1;
        self.t[r] = (self.f)(&self.t[r], &value);
      }
      l >>= 1;
      r >>= 1;
    }
  }
  pub fn index(&self, zero: T, index: usize) -> T {
    let mut res = zero;
    let mut i = index + self.n;
    while i > 0 {
      res = (self.f)(&res, &self.t[i]);
      i >>= 1;
    }
    res
  }
  pub fn create(n: usize, zero: T, f: F) -> Self {
    Self {
      n,
      f,
      t: vec![zero; 2 * n],
    }
  }
  fn push(&mut self, zero: T) {
    for i in 1..self.n {
      let k = i << 1;
      self.t[k] = (self.f)(&self.t[k], &self.t[i]);
      self.t[k + 1] = (self.f)(&self.t[k + 1], &self.t[i]);
      self.t[i] = zero.clone();
    }
  }
  pub fn force(&mut self, zero: T) -> Vec<T> {
    self.push(zero);
    self.t[self.n..2 * self.n].to_vec()
  }
}
