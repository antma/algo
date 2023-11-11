pub fn compute_z_function<T: Eq>(s: &[T]) -> Vec<usize> {
  let n = s.len();
  let mut l = 0;
  let mut r = 0;
  let mut z = Vec::with_capacity(n);
  z.push(0);
  for i in 1..n {
    let mut k = if i <= r { (r - i + 1).min(z[i - l]) } else { 0 };
    while i + k < n && s[k] == s[i + k] {
      k += 1;
    }
    let j = i + k - 1;
    if j > r {
      l = i;
      r = j;
    }
    z.push(k);
  }
  z
}

pub fn compute_prefix_function<T: Eq>(s: &[T]) -> Vec<usize> {
  let n = s.len();
  let mut p = Vec::with_capacity(n);
  p.push(0);
  let mut k = 0;
  for q in 1..n {
    while k > 0 && s[k] != s[q] {
      k = p[k - 1];
    }
    if s[k] == s[q] {
      k += 1;
    }
    p.push(k);
  }
  p
}

fn kmp_next<T: Eq>(s: &[T], p: &Vec<usize>, state: usize, c: T) -> (bool, usize) {
  let mut q = state;
  while q > 0 && c != s[q] {
    q = p[q - 1];
  }
  if c == s[q] {
    q += 1;
  }
  if q == s.len() {
    (true, p[q - 1])
  } else {
    (false, q)
  }
}

pub struct KMPIterator<'a, T> {
  s: &'a [T],
  p: &'a Vec<usize>,
  t: &'a [T],
  state: usize,
  pos: usize,
}

impl<'a, T: Eq + Copy> Iterator for KMPIterator<'a, T> {
  type Item = usize;
  fn next(&mut self) -> Option<Self::Item> {
    let mut q = self.state;
    let mut r: Option<Self::Item> = None;
    for (i, c) in self.t.iter().enumerate().skip(self.pos) {
      let (found, u) = kmp_next(self.s, self.p, q, *c);
      q = u;
      if found {
        r = Some(i + 1 - self.s.len());
        break;
      }
    }
    self.state = q;
    self.pos = match r.as_ref() {
      Some(i) => *i + self.s.len(),
      None => self.t.len(),
    };
    r
  }
}

impl<'a, T> KMPIterator<'a, T> {
  pub fn matches_iterator(s: &'a [T], p: &'a Vec<usize>, t: &'a [T]) -> Self {
    KMPIterator {
      s,
      p,
      t,
      state: 0,
      pos: 0,
    }
  }
}
