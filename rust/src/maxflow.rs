type C = i32;
type E = i32;

#[derive(Clone, Debug)]
struct DListEntry {
  prev: usize,
  next: usize,
}

#[derive(Clone, Debug)]
struct Edge {
  v: usize,
  e: usize,
  f: C,
  c: C,
}

impl Edge {
  fn new(v: usize, e: usize, c: C) -> Self {
    Edge { v, e, f: 0, c }
  }
  fn zero() -> Self {
    Self::new(0, 0, 0)
  }
}

pub struct PushRelabelMaxFlowGraph {
  edges: Vec<Vec<Edge>>,
  current: Vec<usize>,
  h: Vec<i32>,
  nl: Vec<i32>,
  e: Vec<E>,
  dl: Vec<DListEntry>,
  gc: Vec<i32>,
  n: usize,
  maxh: i32,
}

impl PushRelabelMaxFlowGraph {
  pub fn new(n: usize) -> Self {
    PushRelabelMaxFlowGraph {
      edges: vec![vec![Edge::zero(); 0]; n],
      current: vec![std::usize::MAX; n],
      h: vec![0; n],
      nl: vec![-1; n],
      e: vec![0; n],
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
    self.add_link(self.dl[self.n + ht].prev, v);
    self.add_link(v, self.n + ht);
  }
  fn remove(&mut self, i: usize) {
    self.add_link(self.dl[i].prev, self.dl[i].next);
  }
  fn init_preflow(&mut self) {
    let n = self.n;
    for i in n..2 * n {
      self.dl[i].prev = i;
      self.dl[i].next = i;
    }
    self.h[0] = n as i32;
    for k in 0..self.edges[0].len() {
      let p = &mut self.edges[0][k];
      let i = p.v;
      let c = p.c;
      let e = p.e;
      p.f = c;
      if i > 0 && i < n - 1 && 0 == self.e[i] {
        self.insert(0, i);
      }
      self.e[i] += c;
      self.edges[i][e].f = -c;
    }
    self.gc[0] = (n - 1) as i32;
  }
  fn push(&mut self, i: usize, e: usize) {
    let p = &mut self.edges[i][e];
    let j = p.v;
    let d = std::cmp::min(self.e[i], p.c - p.f);
    let pe = p.e;
    p.f += d;
    self.edges[j][pe].f = -p.f;
    self.e[i] -= d;
    self.e[j] += d;
  }
  fn lift(&mut self, i: usize) {
    let mut m = i32::MAX;
    for p in &self.edges[i] {
      if p.f < p.c && m > self.h[p.v] {
        m = self.h[p.v];
      }
    }
    self.h[i] = m + 1;
  }
  fn discharge(&mut self, i: usize) {
    let n = self.n;
    while self.e[i] > 0 {
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
          if self.h[i] < (n as i32) {
            self.gc[self.h[i] as usize] += 1;
            self.insert(self.h[i] as usize, i);
            self.nl[self.h[i] as usize] = if self.dl[maxh as usize + n].next >= n {
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
        let p = &self.edges[i][e];
        let j = p.v;
        let pf = p.f;
        let pc = p.c;
        if self.h[i] == self.h[j] + 1 && pf < pc {
          let aj = self.e[j] <= 0;
          self.push(i, e);
          if aj && j > 0 && j < n - 1 && self.e[j] > 0 {
            self.insert((self.maxh - 1) as usize, j);
            if self.nl[self.maxh as usize] != self.maxh - 1 {
              self.nl[(self.maxh - 1) as usize] = self.nl[self.maxh as usize];
              self.nl[self.maxh as usize] = self.maxh - 1;
            }
          }
          if self.e[i] <= 0 {
            self.remove(i);
            if self.dl[(self.maxh as usize) + n].next >= n {
              self.maxh = self.nl[self.maxh as usize];
            }
          }
        } else {
          self.current[i] += 1;
        }
      }
    }
  }
  pub fn add_edge(&mut self, i: usize, j: usize, w1: C, w2: C) {
    let ei = self.edges[i].len();
    let ej = self.edges[j].len();
    self.edges[i].push(Edge::new(j, ej, w1));
    self.edges[j].push(Edge::new(i, ei, w2));
  }
  pub fn max_flow(&mut self) -> E {
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
