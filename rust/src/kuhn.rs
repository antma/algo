pub struct BipartiteMatching {
  n: usize,
  a: Vec<Vec<u32>>,
  boys: Vec<i32>,
  girls: Vec<i32>,
  used: Vec<bool>,
}

impl BipartiteMatching {
  pub fn new(n: usize, m: usize) -> Self {
    BipartiteMatching {
      n,
      a: vec![Vec::new(); n],
      boys: vec![-1; n],
      girls: vec![-1; m],
      used: vec![false; n],
    }
  }
  pub fn add_edge(&mut self, i: usize, j: usize) {
    self.a[i].push(j as u32);
  }
  fn go(&mut self, i: usize) -> bool {
    if !self.used[i] {
      self.used[i] = true;
      for k in 0..self.a[i].len() {
        let j = self.a[i][k] as usize;
        let g = self.girls[j];
        if g < 0 || self.go(g as usize) {
          self.boys[i] = j as i32;
          self.girls[j] = i as i32;
          return true;
        }
      }
    }
    false
  }
  pub fn matching(&mut self) -> usize {
    let mut res = 0usize;
    let mut stop = false;
    while !stop {
      let old = res;
      for v in &mut self.used {
        *v = false;
      }
      for i in 0..self.n {
        if self.boys[i] < 0 && self.go(i) {
          res += 1;
        }
      }
      stop = old == res;
    }
    res
  }
}
