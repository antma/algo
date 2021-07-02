use crate::maxflow::graph::Graph;

use std::collections::VecDeque;

pub struct DinicMaxFlow<C> {
  g: Graph<C>,
  level: Vec<u32>,
  ptr: Vec<usize>,
}

impl<C> DinicMaxFlow<C>
where
  for<'b> C: std::ops::AddAssign<&'b C> + std::ops::SubAssign<&'b C>,
  C: std::ops::Sub<Output = C> + Ord + From<i8> + Clone + Eq,
{
  pub fn new(g: Graph<C>) -> Self {
    let n = g.edges.len();
    Self {
      g,
      level: vec![0; n],
      ptr: vec![0; n],
    }
  }
  fn bfs(&mut self) -> bool {
    let n = self.g.edges.len();
    let mut q = VecDeque::new();
    q.push_back(0);
    while let Some(v) = q.pop_front() {
      let next_level = self.level[v] + 1;
      for k in 0..self.g.edges[v].len() {
        let p = &self.g.edges[v][k];
        if p.c.clone() - p.f.clone() < C::from(1) || self.level[p.v] < n as u32 {
          continue;
        }
        self.level[p.v] = next_level;
        q.push_back(p.v);
      }
    }
    self.level[n - 1] < n as u32
  }
  fn dfs(&mut self, v: usize, pushed: C) -> C {
    if pushed == C::from(0) {
      return C::from(0);
    }
    let n = self.g.edges.len();
    if v + 1 == n {
      return pushed;
    }
    while self.ptr[v] < self.g.edges[v].len() {
      let k = self.ptr[v];
      let (pv, delta) = {
        let p = &self.g.edges[v][k];
        let u = p.v;
        let delta = p.c.clone() - p.f.clone();
        if delta < C::from(1) || self.level[v] + 1 != self.level[u] {
          self.ptr[v] += 1;
          continue;
        }
        (u, delta)
      };
      let delta = self.dfs(pv, pushed.clone().min(delta));
      if delta == C::from(0) {
        self.ptr[v] += 1;
        continue;
      }
      self.g.add_flow(v, k, &delta);
      return delta;
    }
    C::from(0)
  }
  pub fn max_flow(&mut self, infinite_flow: C) -> C {
    let n32 = self.g.edges.len() as u32;
    let mut f = C::from(0);
    loop {
      for p in &mut self.level {
        *p = n32;
      }
      self.level[0] = 0;
      if !self.bfs() {
        break;
      }
      for p in &mut self.ptr {
        *p = 0;
      }
      loop {
        let pushed = self.dfs(0, infinite_flow.clone());
        if pushed == C::from(0) {
          break;
        }
        f += &pushed;
      }
    }
    f
  }
}
