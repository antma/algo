use std::collections::BTreeMap;

pub struct BMultiSet<T> {
  m: BTreeMap<T, u32>,
  elems: usize,
}

impl<T: Ord> Default for BMultiSet<T> {
  fn default() -> Self {
    Self {
      m: BTreeMap::new(),
      elems: 0,
    }
  }
}

impl<T: Ord> BMultiSet<T> {
  pub fn new() -> Self {
    Self::default()
  }
  pub fn is_empty(&self) -> bool {
    self.m.is_empty()
  }
  pub fn len(&self) -> usize {
    self.elems
  }
  pub fn contains(&self, x: &T) -> bool {
    self.m.contains_key(x)
  }
  pub fn first(&self) -> Option<&T> {
    self.m.iter().next().map(|q| q.0)
  }
  pub fn last(&self) -> Option<&T> {
    self.m.iter().next_back().map(|q| q.0)
  }
  pub fn insert(&mut self, x: T) {
    let e = self.m.entry(x).or_insert(0);
    *e += 1;
    self.elems += 1;
  }
  pub fn remove(&mut self, x: &T) {
    let mut b = false;
    if let Some(w) = self.m.get_mut(x) {
      *w -= 1;
      self.elems -= 1;
      if *w == 0 {
        b = true;
      }
    }
    if b {
      self.m.remove(x);
    }
  }
}

impl<T: Ord + Clone> BMultiSet<T> {
  pub fn pop_first(&mut self) -> Option<T> {
    if self.m.is_empty() {
      return None;
    }
    self.elems -= 1;
    if let Some(w) = self.m.iter_mut().next() {
      let x = w.0.clone();
      *w.1 -= 1;
      if *w.1 != 0 {
        return Some(x);
      }
      self.m.remove(&x);
      return Some(x);
    }
    unreachable!();
  }
  pub fn pop_last(&mut self) -> Option<T> {
    if self.m.is_empty() {
      return None;
    }
    self.elems -= 1;
    if let Some(w) = self.m.iter_mut().next_back() {
      let x = w.0.clone();
      *w.1 -= 1;
      if *w.1 != 0 {
        return Some(x);
      }
      self.m.remove(&x);
      return Some(x);
    }
    unreachable!();
  }
}
