pub struct KnuthRandom {
  u: usize,
  v: usize,
  a: [i32; 55],
}

impl KnuthRandom {
  pub fn new(seed: i32) -> Self {
    const M: i32 = 0x7fffffff;
    let mut a = [0; 55];
    let mut j = (0x5EED5EEDi32 >> 3)
      .wrapping_sub(seed)
      .wrapping_abs()
      .max(0)
      % M;
    let mut k = 1;
    a[54] = j;
    for i in 1..55 {
      let idx = (21 * i - 1) % 55;
      a[idx] = k;
      k = j - k;
      if k < 0 {
        k += M;
      }
      j = a[idx];
    }
    for _ in 1..5 {
      for i in 0..55 {
        a[i] -= a[(i + 31) % 55];
        if a[i] < 0 {
          a[i] += M;
        }
      }
    }
    Self { u: 54, v: 30, a }
  }
  pub fn next(&mut self) -> i32 {
    self.u += 1;
    if self.u == 55 {
      self.u = 0;
    }
    self.v += 1;
    if self.v == 55 {
      self.v = 0;
    }
    let mut j = self.a[self.u] - self.a[self.v];
    if j < 0 {
      j += 0x7fffffff;
    }
    self.a[self.u] = j;
    j
  }
  pub fn next_f64(&mut self) -> f64 {
    (self.next() as f64) * (1.0f64 / 2147483647.0f64)
  }
  pub fn normal(&mut self, mean: f64, sigma: f64) -> f64 {
    loop {
      let u1 = self.next_f64();
      let u2 = 1.0 - self.next_f64();
      let z = 1.7155277699214135929603792825575449562 * (u1 - 0.5) / u2;
      if z * z <= -4.0 * (u2.ln()) {
        break mean + z * sigma;
      }
    }
  }
}
