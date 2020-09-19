type T = i32;
const INF: T = T::MAX / 2;
pub struct AssignmentProblem {
  n: usize,
  g: Vec<Vec<T>>,
  s: Vec<bool>,
  d: Vec<bool>,
  e: Vec<bool>,
  a: Vec<i32>,
  b: Vec<i32>,
}

impl AssignmentProblem {
  pub fn new(n: usize, g: Vec<Vec<T>>) -> Self {
    AssignmentProblem {
      n,
      g,
      s: vec![false; n],
      d: vec![false; n],
      e: vec![false; n],
      a: vec![0; n],
      b: vec![0; n],
    }
  }
  fn rec(&mut self, i: usize) -> bool {
    if !self.s[i] {
      self.s[i] = true;
      for j in 0..self.n {
        if self.g[i][j] == 0 && (self.b[j] < 0 || self.rec(self.b[j] as usize)) {
          self.a[i] = j as i32;
          self.b[j] = i as i32;
          return true;
        }
      }
    }
    false
  }
  fn matching(&mut self) -> usize {
    for x in &mut self.a {
      *x = -1;
    }
    for x in &mut self.b {
      *x = -1;
    }
    for x in &mut self.d {
      *x = false;
    }
    for x in &mut self.e {
      *x = false;
    }
    let mut c = 0;
    for i in 0..self.n {
      for x in &mut self.s {
        *x = false;
      }
      if self.rec(i) {
        self.d[i] = true;
        c += 1;
      } else {
        for j in 0..i {
          if self.s[j] {
            self.d[j] = false;
            if self.a[j] >= 0 {
              self.e[self.a[j] as usize] = true;
            }
          }
        }
      }
    }
    c
  }
  pub fn minimize(&mut self) -> T {
    let n = self.n;
    let mut res: T = 0;
    let mut v = INF;
    for i in 0..n {
      v = std::cmp::min(v, *self.g[i].iter().min().unwrap());
    }
    if v < 0 {
      res += v * (n as T);
      for i in 0..n {
        for x in self.g[i].iter_mut() {
          *x -= v;
        }
      }
    }
    loop {
      //eprintln!("res = {}", res);
      for i in 0..n {
        let min = *self.g[i].iter().min().unwrap();
        if min > 0 {
          res += min;
          for j in 0..n {
            self.g[i][j] -= min;
          }
        }
      }
      for j in 0..n {
        let min = (0..n).map(|i| self.g[i][j]).min().unwrap();
        if min > 0 {
          res += min;
          for i in 0..n {
            self.g[i][j] -= min;
          }
        }
      }
      let c = self.matching();
      if c == n {
        break;
      }
      let mut min_g = INF;
      for i in 0..n {
        if !self.d[i] {
          for j in 0..n {
            if !self.e[j] && min_g > self.g[i][j] {
              min_g = self.g[i][j];
            }
          }
        }
      }
      res += min_g * ((n - c) as T);
      for i in 0..n {
        if self.d[i] {
          for x in &mut self.g[i] {
            *x += min_g;
          }
        }
      }
      for j in 0..n {
        if !self.e[j] {
          for i in 0..n {
            self.g[i][j] -= min_g;
          }
        }
      }
    }
    res
  }
}
