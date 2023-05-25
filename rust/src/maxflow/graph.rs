#[derive(Clone)]
pub struct Edge<C> {
  pub v: usize,
  pub e: usize,
  pub f: C,
  pub c: C,
}

impl<C: From<i8>> Edge<C> {
  fn new(v: usize, e: usize, c: C) -> Self {
    Edge {
      v,
      e,
      f: C::from(0),
      c,
    }
  }
}

pub struct Graph<C> {
  pub edges: Vec<Vec<Edge<C>>>,
}

impl<C> Graph<C>
where
  C: From<i8> + Copy + Ord + std::ops::AddAssign<C> + std::ops::SubAssign<C>,
{
  pub fn new(n: usize) -> Self {
    Self {
      edges: vec![Vec::new(); n],
    }
  }
  pub fn add_flow(&mut self, v: usize, k: usize, delta: C) {
    let (u, l) = {
      let p = &mut self.edges[v][k];
      p.f += delta;
      (p.v, p.e)
    };
    self.edges[u][l].f -= delta;
  }
  pub fn add_edge(&mut self, i: usize, j: usize, w1: C, w2: C) {
    if w1 > C::from(0) {
      let ei = self.edges[i].len();
      let ej = self.edges[j].len();
      self.edges[i].push(Edge::new(j, ej, w1));
      self.edges[j].push(Edge::new(i, ei, w2));
    }
  }
}
