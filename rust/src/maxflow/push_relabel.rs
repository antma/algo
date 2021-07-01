use crate::maxflow::graph::{Edge, Graph};

use std::ops::{AddAssign, Neg, Sub, SubAssign};

#[derive(Clone, Debug)]
struct DListEntry {
  prev: usize,
  next: usize,
}

pub struct PushRelabelMaxFlow<C> {
  edges: Vec<Vec<Edge<C>>>,
  current: Vec<usize>,
  h: Vec<i32>,
  nl: Vec<i32>,
  e: Vec<C>,
  dl: Vec<DListEntry>,
  gc: Vec<i32>,
  n: usize,
  maxh: i32,
}

impl<C> PushRelabelMaxFlow<C>
where
  C: From<i8> + Copy + Clone + Eq + Ord + AddAssign + SubAssign + Neg<Output = C> + Sub<Output = C>,
{
  pub fn new(graph: Graph<C>) -> Self {
    let n = graph.edges.len();
    PushRelabelMaxFlow {
      edges: graph.edges,
      current: vec![0xffffffffusize; n],
      h: vec![0; n],
      nl: vec![-1; n],
      e: vec![C::from(0); n],
      dl: vec![DListEntry { prev: 0, next: 0 }; 2 * n],
      gc: vec![0; n],
      n,
      maxh: 0,
    }
  }
  fn add_link(&mut self, u: usize, v: usize) {
    self.dl[u].next = v;
    self.dl[v].prev = u;
  }
  fn insert(&mut self, ht: usize, v: usize) {
    let n = self.n;
    let u = self.dl[n + ht].prev;
    self.add_link(u, v);
    self.add_link(v, n + ht);
  }
  fn remove(&mut self, i: usize) {
    let u = self.dl[i].prev;
    let v = self.dl[i].next;
    self.add_link(u, v);
  }
  fn init_preflow(&mut self) {
    let n = self.n;
    for i in n..2 * n {
      self.dl[i].prev = i;
      self.dl[i].next = i;
    }
    self.h[0] = n as i32;
    for k in 0..self.edges[0].len() {
      let Edge { v: i, c, e, .. } = self.edges[0][k];
      self.edges[0][k].f = c;
      if i > 0 && i < n - 1 && C::from(0) == self.e[i] {
        self.insert(0, i);
      }
      self.e[i] += c;
      self.edges[i][e].f = -c;
    }
    self.gc[0] = (n - 1) as i32;
  }
  fn push(&mut self, i: usize, e: usize) {
    let Edge { v: j, c, e: pe, f } = self.edges[i][e];
    let d = self.e[i].min(c - f);
    self.edges[i][e].f += d;
    self.edges[j][pe].f -= d;
    self.e[i] -= d;
    self.e[j] += d;
  }
  fn lift(&mut self, i: usize) {
    let mut m = 0x7fffffffi32;
    for p in &self.edges[i] {
      if p.f < p.c && m > self.h[p.v] {
        m = self.h[p.v];
      }
    }
    self.h[i] = m + 1;
  }
  fn discharge(&mut self, i: usize) {
    let n = self.n;
    while self.e[i] > C::from(0) {
      if self.current[i] >= self.edges[i].len() {
        self.current[i] = 0;
        self.gc[self.maxh as usize] -= 1;
        if 0 == self.gc[self.maxh as usize] && self.maxh > 0 && self.maxh < (n as i32) {
          self.remove(i);
          self.h[i] = (n + 1) as i32;
          for k in 1..n {
            if self.h[k] > (self.maxh as i32) && self.h[k] <= (n as i32) {
              self.gc[self.h[k] as usize] -= 1;
              self.h[k] = (n + 1) as i32;
            }
          }
          self.maxh = self.nl[self.maxh as usize];
          break;
        } else {
          let maxh = self.maxh;
          self.lift(i);
          self.remove(i);
          let hi = self.h[i] as usize;
          if hi < n {
            self.gc[hi] += 1;
            self.insert(hi, i);
            self.nl[hi] = if self.dl[maxh as usize + n].next >= n {
              self.nl[maxh as usize]
            } else {
              maxh
            };
            self.maxh = self.h[i];
          } else {
            if self.dl[maxh as usize + n].next >= n {
              self.maxh = self.nl[maxh as usize];
            }
            break;
          }
        }
      } else {
        let e = self.current[i];
        let Edge {
          v: j, f: pf, c: pc, ..
        } = self.edges[i][e];
        if self.h[i] == self.h[j] + 1 && pf < pc {
          let aj = self.e[j] <= C::from(0);
          self.push(i, e);
          let maxh = self.maxh as usize;
          if aj && j > 0 && j < n - 1 && self.e[j] > C::from(0) {
            self.insert(maxh - 1, j);
            if self.nl[maxh] != (maxh - 1) as i32 {
              self.nl[maxh - 1] = self.nl[maxh];
              self.nl[maxh] = (maxh - 1) as i32;
            }
          }
          if self.e[i] <= C::from(0) {
            self.remove(i);
            if self.dl[maxh + n].next >= n {
              self.maxh = self.nl[maxh];
            }
          }
        } else {
          self.current[i] += 1;
        }
      }
    }
  }
  pub fn max_flow(&mut self) -> C {
    let n = self.n;
    self.init_preflow();
    for i in 1..n - 1 {
      self.current[i] = 0;
    }
    self.maxh = 0;
    while self.maxh >= 0 {
      let v = self.dl[(self.maxh as usize) + n].next;
      if v >= n {
        break;
      }
      self.discharge(v);
    }
    self.e[n - 1]
  }
}
