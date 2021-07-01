use crate::maxflow::graph::{Edge, Graph};

use std::collections::VecDeque;
use std::ops::{AddAssign, Sub, SubAssign};

pub struct DinicMaxFlow<C> {
  edges: Vec<Vec<Edge<C>>>,
  level: Vec<u32>,
  ptr: Vec<usize>,
  q: VecDeque<usize>,
}

impl<C> DinicMaxFlow<C>
where
  for<'b> C: AddAssign<&'b C> + SubAssign<&'b C>,
  C: Sub<Output = C> + Ord + From<i8> + Clone + Eq,
{
  pub fn new(graph: Graph<C>) -> Self {
    let edges = graph.edges;
    let n = edges.len();
    Self {
      edges,
      level: vec![0; n],
      ptr: vec![0; n],
      q: VecDeque::new(),
    }
  }
  fn bfs(&mut self) -> bool {
    let n = self.edges.len();
    while let Some(v) = self.q.pop_front() {
      let next_level = self.level[v] + 1;
      for k in 0..self.edges[v].len() {
        let p = &self.edges[v][k];
        if p.c.clone() - p.f.clone() < C::from(1) || self.level[p.v] < n as u32 {
          continue;
        }
        self.level[p.v] = next_level;
        self.q.push_back(p.v);
      }
    }
    self.level[n - 1] < n as u32
  }
  fn dfs(&mut self, v: usize, pushed: C) -> C {
    if pushed == C::from(0) {
      return C::from(0);
    }
    let n = self.edges.len();
    if v + 1 == n {
      return pushed;
    }
    while self.ptr[v] < self.edges[v].len() {
      let k = self.ptr[v];
      let (pv, pe, delta) = {
        let p = &self.edges[v][k];
        let u = p.v;
        let delta = p.c.clone() - p.f.clone();
        if delta < C::from(1) || self.level[v] + 1 != self.level[u] {
          self.ptr[v] += 1;
          continue;
        }
        (u, p.e.clone(), delta)
      };
      let tr = self.dfs(pv, pushed.clone().min(delta));
      if tr == C::from(0) {
        self.ptr[v] += 1;
        continue;
      }
      self.edges[v][k].f += &tr;
      self.edges[pv][pe].f -= &tr;
      return tr;
    }
    C::from(0)
  }
  pub fn max_flow(&mut self, infinite_flow: C) -> C {
    let n32 = self.edges.len() as u32;
    let mut f = C::from(0);
    loop {
      for p in &mut self.level {
        *p = n32;
      }
      self.level[0] = 0;
      self.q.push_back(0);
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
