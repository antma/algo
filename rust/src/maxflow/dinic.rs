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
  C: std::ops::Sub<Output = C> + Ord + From<i8> + Copy + Eq,
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
        if p.c - p.f < C::from(1) || self.level[p.v] < n as u32 {
          continue;
        }
        self.level[p.v] = next_level;
        q.push_back(p.v);
      }
    }
    self.level[n - 1] < n as u32
  }
  fn dfs(&mut self, infinite_flow: C) -> C {
    let n = self.g.edges.len();
    let sink = n - 1;
    let mut s = Vec::new();
    s.push((0, infinite_flow));
    while let Some((v, pushed)) = s.pop() {
      if v == sink {
        for (v, _) in s {
          self.g.add_flow(v, self.ptr[v] - 1, &pushed);
        }
        return pushed;
      }
      loop {
        let k = self.ptr[v];
        if k >= self.g.edges[v].len() {
          break;
        }
        self.ptr[v] += 1;
        let (pv, delta) = {
          let p = &self.g.edges[v][k];
          let delta = p.c - p.f;
          if delta < C::from(1) || self.level[v] + 1 != self.level[p.v] {
            continue;
          }
          (p.v, delta)
        };
        s.push((v, pushed));
        s.push((pv, pushed.min(delta)));
        break;
      }
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
        let pushed = self.dfs(infinite_flow);
        if pushed == C::from(0) {
          break;
        }
        f += &pushed;
      }
    }
    f
  }
}
