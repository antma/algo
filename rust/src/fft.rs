pub mod ntt;

pub fn pad_vec_for_fft<T: Clone>(a: &mut Vec<T>, zero: T) {
  a.resize(a.len().next_power_of_two(), zero);
}

pub struct Revbin {
  tbl: Vec<u32>,
  n: usize,
  h: usize,
}

impl Revbin {
  pub fn new(n: usize) -> Self {
    Revbin {
      tbl: vec![u32::MAX; n],
      n,
      h: n >> 1,
    }
  }
  fn update(&mut self, k: usize) -> usize {
    if self.tbl[k] != u32::MAX {
      return self.tbl[k] as usize;
    }
    let mut r = k;
    let mut i = self.h;
    loop {
      r ^= i;
      if (r & i) != 0 {
        break;
      }
      i >>= 1;
    }
    self.tbl[k] = r as u32;
    r
  }
  pub fn permute<T>(&mut self, a: &mut [T]) {
    let n = self.n;
    if n <= 2 {
      return;
    }
    let mut r = 0;
    let mut x = 1;
    let n1 = n - 1;
    while x < self.h {
      r += self.h;
      a.swap(x, r);
      x += 1;
      r = self.update(r);
      if r > x {
        a.swap(x, r);
        a.swap(n1 - x, n1 - r);
      }
      x += 1;
    }
  }
}
