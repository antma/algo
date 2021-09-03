pub struct FenwickTree<T> {
  a: Vec<T>,
  n: usize,
}

impl<T> FenwickTree<T>
where
  T: std::ops::AddAssign + Copy + From<bool>,
{
  pub fn new(n: usize) -> Self {
    Self {
      a: vec![T::from(false); n],
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
    let mut r = T::from(false);
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

//////////////////// FenwickTree2D ////////////////////
struct FenwickUpdateIterator(usize, usize);
impl Iterator for FenwickUpdateIterator {
  type Item = usize;
  fn next(&mut self) -> Option<Self::Item> {
    if self.0 >= self.1 {
      None
    } else {
      let i = self.0;
      self.0 |= i + 1;
      Some(i)
    }
  }
}

struct FenwickReduceIterator(isize);
impl Iterator for FenwickReduceIterator {
  type Item = usize;
  fn next(&mut self) -> Option<Self::Item> {
    if self.0 < 0 {
      None
    } else {
      let i = self.0;
      self.0 = (i & (i + 1)) - 1;
      Some(i as usize)
    }
  }
}

pub struct FenwickTree2D<T> {
  a: Vec<Vec<T>>,
  nx: usize,
  ny: usize,
}

impl<T> FenwickTree2D<T>
where
  T: std::ops::Add<Output = T> + Copy + From<bool>,
{
  pub fn new(nx: usize, ny: usize) -> Self {
    Self {
      a: vec![vec![T::from(false); ny]; nx],
      nx,
      ny,
    }
  }
  pub fn update(&mut self, x: usize, y: usize, v: T) {
    let ny = self.ny;
    FenwickUpdateIterator(x, self.nx).for_each(|x| {
      let w = &mut self.a[x];
      FenwickUpdateIterator(y, ny).for_each(|y| {
        let q = &mut w[y];
        *q = *q + v;
      })
    });
  }
  //sum on [0, x) x [0, y)
  pub fn reduce(&self, x: usize, y: usize) -> T {
    FenwickReduceIterator(x as isize - 1).fold(T::from(false), |acc, i| {
      let w = &self.a[i];
      FenwickReduceIterator(y as isize - 1).fold(acc, |acc2, j| acc2 + w[j])
    })
  }
}
