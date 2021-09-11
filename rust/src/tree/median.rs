use crate::tree::bmultiset::BMultiSet;

pub struct DinamicMedian<T> {
  left: BMultiSet<T>,
  right: BMultiSet<T>,
}

impl<T: Ord> Default for DinamicMedian<T> {
  fn default() -> Self {
    Self {
      left: BMultiSet::new(),
      right: BMultiSet::new(),
    }
  }
}

impl<T: Ord> DinamicMedian<T> {
  pub fn new() -> Self {
    Self::default()
  }
  pub fn len(&self) -> usize {
    self.left.len() + self.right.len()
  }
  pub fn get(&self) -> Vec<&T> {
    let mut r = Vec::new();
    if let Some(w) = self.left.last() {
      r.push(w);
    }
    if self.left.len() == self.right.len() {
      if let Some(w) = self.right.first() {
        r.push(w);
      }
    }
    r
  }
}

impl<T: Ord + Clone> DinamicMedian<T> {
  fn fix(&mut self) {
    if self.left.len() > self.right.len() + 1 {
      let x = self.left.pop_last().unwrap();
      self.right.insert(x);
    } else if self.left.len() < self.right.len() {
      let x = self.right.pop_first().unwrap();
      self.left.insert(x);
    }
  }
  pub fn insert(&mut self, x: T) {
    let mut l = true;
    if let Some(w) = self.left.last() {
      if *w <= x {
        l = false;
      }
    }
    if l {
      self.left.insert(x);
    } else {
      self.right.insert(x);
    }
    self.fix();
  }
  pub fn remove(&mut self, x: &T) {
    let mut l = false;
    if let Some(w) = self.left.last() {
      if *w >= *x {
        l = true;
      }
    }
    if l {
      self.left.remove(x);
    } else {
      self.right.remove(x);
    }
    self.fix();
  }
}
