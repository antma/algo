//idmap
use std::collections::HashMap;
pub struct IdMap<T> {
  h: HashMap<T, u32>,
  v: Vec<T>,
}

impl<T: Clone + std::cmp::Eq + std::hash::Hash> IdMap<T> {
  pub fn get(&mut self, x: &T) -> u32 {
    if let Some(v) = self.h.get(&x) {
      return *v;
    }
    let n = self.v.len() as u32;
    self.h.insert(x.clone(), n);
    self.v.push(x.clone());
    n
  }
  pub fn new() -> Self {
    IdMap {
      h: HashMap::new(),
      v: Vec::new(),
    }
  }
  pub fn len(&self) -> usize {
    self.v.len()
  }
}
