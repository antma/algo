pub struct KnuthRandom {
  u: usize,
  v: usize,
  a: [i32; 55],
}

impl KnuthRandom {
  const M: i32 = 0x7fff_ffff;
  const F: f64 = 1.0 / (KnuthRandom::M as f64);
  pub fn new(seed: i32) -> Self {
    let mut a = [0; 55];
    let mut j = (0x5EED_5EEDi32 >> 3)
      .wrapping_sub(seed)
      .wrapping_abs()
      .max(0)
      % KnuthRandom::M;
    let mut k = 1;
    a[54] = j;
    for i in 1..55 {
      let idx = (21 * i - 1) % 55;
      a[idx] = k;
      k = j - k;
      if k < 0 {
        k += KnuthRandom::M;
      }
      j = a[idx];
    }
    for _ in 1..5 {
      for i in 0..55 {
        a[i] -= a[(i + 31) % 55];
        if a[i] < 0 {
          a[i] += KnuthRandom::M;
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
      j += KnuthRandom::M;
    }
    self.a[self.u] = j;
    j
  }
  pub fn random(&mut self) -> f64 {
    (self.next() as f64) * KnuthRandom::F
  }
  pub fn random_precise(&mut self) -> f64 {
    let f1 = self.random();
    let f2 = self.random();
    f1 * KnuthRandom::F + f2
  }
  fn random_usize(&mut self, n: usize) -> usize {
    let f = if n <= KnuthRandom::M as usize {
      self.random()
    } else {
      self.random_precise()
    };
    (f * n as f64) as usize
  }
  pub fn randrange<T, I: Iterator<Item = T> + ExactSizeIterator>(&mut self, rng: I) -> T {
    let l = rng.len();
    let mut it = rng;
    it.nth(self.random_usize(l)).unwrap()
  }
  pub fn shuffle<T>(&mut self, a: &mut [T]) {
    let n = a.len();
    if n <= 1 {
      return;
    }
    for i in 0..n - 1 {
      let j = self.randrange(i..n);
      if i != j {
        a.swap(i, j);
      }
    }
  }
  pub fn normal(&mut self, mean: f64, sigma: f64) -> f64 {
    loop {
      let u1 = self.random();
      let u2 = 1.0 - self.random();
      let z = 1.715_527_769_921_413_593 * (u1 - 0.5) / u2;
      if z * z <= -4.0 * (u2.ln()) {
        break mean + z * sigma;
      }
    }
  }
}
