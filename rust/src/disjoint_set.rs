pub struct DisjointSetWithSize {
  p: Vec<usize>,
  h: Vec<i32>,
  s: Vec<u32>,
}

impl DisjointSetWithSize {
  pub fn new(n: usize) -> Self {
    Self {
      p: (0..n).collect(),
      h: vec![0i32; n],
      s: vec![1u32; n],
    }
  }
  pub fn find_set(&mut self, x: usize) -> usize {
    let y = self.p[x];
    if y == x {
      return x;
    }
    self.p[x] = self.find_set(y);
    return self.p[x];
  }
  pub fn merge(&mut self, x: usize, y: usize) -> bool {
    let i = self.find_set(x);
    let j = self.find_set(y);
    if i != j {
      match self.h[i].cmp(&self.h[j]) {
        std::cmp::Ordering::Less => {
          self.s[j] += self.s[i];
          self.p[i] = j;
        }
        std::cmp::Ordering::Greater => {
          self.s[i] += self.s[j];
          self.p[j] = i;
        }
        _ => {
          self.s[j] += self.s[i];
          self.p[i] = j;
          self.h[j] += 1;
        }
      }
      true
    } else {
      false
    }
  }
  pub fn size(&mut self, x: usize) -> u32 {
    let i = self.find_set(x);
    self.s[i]
  }
}
