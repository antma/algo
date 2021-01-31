pub struct FenwickTree<T> {
  a: Vec<T>,
  n: usize,
}

impl<T> FenwickTree<T>
where
  T: std::ops::AddAssign + Copy + From<i8>,
{
  pub fn new(n: usize) -> Self {
    Self {
      a: vec![T::from(0); n],
      n,
    }
  }
  pub fn update(&mut self, x: usize, v: T) {
    let n = self.n;
    let mut i = x;
    while i < n {
      self.a[i] += v;
      i |= i + 1;
    }
  }
  //sum on [0, x)
  pub fn reduce(&self, x: usize) -> T {
    let mut r = T::from(0);
    if x == 0 {
      return r;
    }
    let mut i = x - 1;
    loop {
      r += self.a[i];
      i &= i + 1;
      if i == 0 {
        break r;
      }
      i -= 1;
    }
  }
}
