pub trait MoState<E, R> {
  fn fast_reset(&self) -> bool;
  fn reset(&mut self);
  fn add_left(&mut self, l: usize, r: usize);
  fn add_right(&mut self, l: usize, r: usize);
  fn del_left(&mut self, l: usize, r: usize);
  fn del_right(&mut self, l: usize, r: usize);
  fn result(&self, e: &E) -> R;
}

const HILBERT_D: [i32; 4] = [3, 0, 0, 1];
fn hilbert(x: i32, y: i32, k: i32, a: i32) -> i64 {
  if k < 0 {
    0
  } else {
    let m = !(1 << k);
    let o = 3 & (((x >> k) ^ 3 * (y >> k)) + a);
    let b = hilbert(x & m, y & m, k - 1, a + HILBERT_D[o as usize]);
    let ss = 1i64 << (2 * k);
    (if o == 1 || o == 2 { b } else { ss - (b + 1) }) + ss * (o as i64)
  }
}

fn mo_make_idx<E>(queries: &Vec<(std::ops::Range<usize>, E)>) -> Vec<usize> {
  if queries.is_empty() {
    return Vec::new();
  }
  let v = queries
    .iter()
    .max_by(|u, v| u.0.end.cmp(&v.0.end))
    .unwrap()
    .0
    .end;
  let t = if v > 0 {
    8 * std::mem::size_of::<usize>() as u32 - (v - 1).leading_zeros()
  } else {
    0
  } as i32;
  let mut x = Vec::with_capacity(queries.len());
  for q in queries {
    x.push(hilbert(q.0.start as i32, q.0.end as i32 - 1, t, 0));
  }
  let mut idx = (0..queries.len()).collect::<Vec<_>>();
  idx.sort_unstable_by_key(move |i| x[*i]);
  idx
}

pub fn mo_process_queries<E, R: Default + Clone, S: MoState<E, R>>(
  s: &mut S,
  queries: &Vec<(std::ops::Range<usize>, E)>,
) -> Vec<R> {
  let fast_reset = s.fast_reset();
  let mut l = 0;
  let mut r = 0;
  let mut res = vec![R::default(); queries.len()];
  for j in mo_make_idx(&queries) {
    let q = &queries[j];
    if l >= r {
      l = q.0.start;
      r = l;
      s.reset();
    } else if l >= q.0.end || r <= q.0.start {
      if fast_reset {
        s.reset();
      } else {
        s.del_left(l, r);
      }
      l = q.0.start;
      r = l;
    }
    if r < q.0.end {
      s.add_right(r, q.0.end);
      r = q.0.end;
    } else if r > q.0.end {
      s.del_right(q.0.end, r);
      r = q.0.end;
    }
    if l > q.0.start {
      s.add_left(q.0.start, l);
      l = q.0.start;
    } else if l < q.0.start {
      s.del_left(l, q.0.start);
      l = q.0.start;
    }
    res[j] = s.result(&q.1);
  }
  res
}

struct Void;

pub struct MoCountUniqueElements {
  u: usize,
  a: Vec<u32>,
  cnt: Vec<u32>,
}

impl MoState<Void, usize> for MoCountUniqueElements {
  fn fast_reset(&self) -> bool {
    return false;
  }
  fn reset(&mut self) {
    for q in &mut self.cnt {
      *q = 0;
    }
    self.u = 0;
  }
  fn add_left(&mut self, l: usize, r: usize) {
    self.add(l, r);
  }
  fn add_right(&mut self, l: usize, r: usize) {
    self.add(l, r);
  }
  fn del_left(&mut self, l: usize, r: usize) {
    self.del(l, r);
  }
  fn del_right(&mut self, l: usize, r: usize) {
    self.del(l, r);
  }
  fn result(&self, _e: &Void) -> usize {
    self.u
  }
}

impl MoCountUniqueElements {
  pub fn new(a: Vec<u32>, m: usize) -> Self {
    Self {
      u: 0,
      a,
      cnt: vec![0; m],
    }
  }
  fn add(&mut self, l: usize, r: usize) {
    for i in self.a[l..r].iter() {
      let i = *i as usize;
      if self.cnt[i] == 0 {
        self.u += 1;
      }
      self.cnt[i] += 1;
    }
  }
  fn del(&mut self, l: usize, r: usize) {
    for i in self.a[l..r].iter() {
      let i = *i as usize;
      if self.cnt[i] == 1 {
        self.u -= 1;
      }
      self.cnt[i] -= 1;
    }
  }
}
