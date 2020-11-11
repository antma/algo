pub struct FHT {
  tbl: Vec<u32>,
  n: usize,
  h: usize,
  ldn: usize,
}

impl FHT {
  pub fn new(n: usize) -> Self {
    let h = n >> 1;
    let mut ldn = 0;
    while (1 << ldn) != n {
      ldn += 1;
    }
    Self {
      tbl: vec![u32::MAX; n],
      n,
      h,
      ldn,
    }
  }
  fn revbin_update(&mut self, k: usize) -> usize {
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
  fn revbin_permute(&mut self, a: &mut Vec<f64>) {
    if self.n <= 2 {
      return;
    }
    let mut r = 0;
    let mut x = 1;
    let n1 = self.n - 1;
    while x < self.h {
      r += self.h;
      a.swap(x, r);
      x += 1;
      r = self.revbin_update(r);
      if r > x {
        a.swap(x, r);
        a.swap(n1 - x, n1 - r);
      }
      x += 1;
    }
  }
  fn fht(&mut self, a: &mut Vec<f64>) {
    if self.ldn < 1 {
      return;
    }
    self.revbin_permute(a);
    for ldm in 1..=self.ldn {
      let m = 1 << ldm;
      let mh = m >> 1;
      let m4 = m >> 2;
      let delta = std::f64::consts::PI / (mh as f64);
      let mut b = mh;
      let mut r = 0;
      while r < self.n {
        let d = &mut a[b..];
        for j in 1..m4 {
          let u = d[j];
          let v = d[mh - j];
          let (s, c) = ((j as f64) * delta).sin_cos();
          d[j] = c * u + s * v;
          d[mh - j] = s * u - c * v;
        }
        for j in b..mh + b {
          let v = a[j];
          let u = a[j - mh];
          a[j - mh] += v;
          a[j] = u - v;
        }
        r += m;
        b += m;
      }
    }
  }
  fn normalize(&self, a: &mut Vec<f64>) {
    let x = 1.0 / (self.n as f64);
    for y in a.iter_mut() {
      *y *= x;
    }
  }
  //y := conv (x, y)
  pub fn conv(&mut self, x: &mut Vec<f64>, y: &mut Vec<f64>) {
    self.fht(x);
    self.fht(y);
    let mut i = 1;
    let mut j = self.n - 1;
    while i < j {
      let xi = x[i];
      let xj = x[j];
      let yp = y[i] + y[j];
      let ym = y[i] - y[j];
      y[i] = (xi * yp + xj * ym) * 0.5;
      y[j] = (xj * yp - xi * ym) * 0.5;
      i += 1;
      j -= 1;
    }
    y[0] *= x[0];
    if self.n > 1 {
      y[self.n >> 1] *= x[self.n >> 1];
    }
    self.fht(y);
    self.normalize(y);
  }
}
