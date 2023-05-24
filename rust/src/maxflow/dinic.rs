use crate::maxflow::graph::Graph;

pub struct DinicMaxFlow<C>(pub Graph<C>);

impl<C> DinicMaxFlow<C>
where
  C: std::ops::Sub<Output = C>
    + Ord
    + From<bool>
    + Copy
    + Eq
    + std::ops::AddAssign<C>
    + std::ops::SubAssign<C>,
{
  fn bfs(&mut self, q: &mut Vec<u32>, level: &mut Vec<u32>) -> bool {
    let mut left = 0;
    let mut right = 1;
    q[0] = 0;
    let n = self.0.edges.len();
    let n32 = n as u32;
    if level.is_empty() {
      for _ in 0..n {
        level.push(n32);
      }
    } else {
      for p in level.iter_mut() {
        *p = n32;
      }
    }
    level[0] = 0;
    while left < right {
      let v = q[left] as usize;
      left += 1;
      let next_level = level[v] + 1;
      for p in &self.0.edges[v] {
        if p.c <= p.f || level[p.v] < n32 {
          continue;
        }
        level[p.v] = next_level;
        q[right] = p.v as u32;
        right += 1;
      }
    }
    level[n - 1] < n32
  }
  fn dfs(&mut self, infinite_flow: C, level: &Vec<u32>, ptr: &mut Vec<u32>) -> C {
    let n = self.0.edges.len();
    let sink = n - 1;
    let mut s = Vec::new();
    s.push((0, infinite_flow));
    while let Some((v, pushed)) = s.pop() {
      if v == sink {
        for (v, _) in s {
          self.0.add_flow(v, ptr[v] as usize - 1, pushed);
        }
        return pushed;
      }
      let next_level = level[v] + 1;
      let e = &self.0.edges[v];
      loop {
        let k = ptr[v] as usize;
        if k >= e.len() {
          break;
        }
        ptr[v] += 1;
        let p = &e[k];
        let delta = p.c - p.f;
        if delta <= C::from(false) || next_level != level[p.v] {
          continue;
        }
        s.push((v, pushed));
        s.push((p.v, pushed.min(delta)));
        break;
      }
    }
    C::from(false)
  }
  pub fn max_flow(&mut self, infinite_flow: C) -> C {
    let n = self.0.edges.len();
    let mut f = C::from(false);
    let mut level = Vec::with_capacity(n);
    let mut q = vec![0u32; n];
    let mut ptr = vec![0u32; n];
    while self.bfs(&mut q, &mut level) {
      for p in &mut ptr {
        *p = 0;
      }
      loop {
        let pushed = self.dfs(infinite_flow, &level, &mut ptr);
        if pushed == C::from(false) {
          break;
        }
        f += pushed;
      }
    }
    f
  }
}
