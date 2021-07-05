pub fn addm(x: u32, y: u32, m: u32) -> u32 {
  let r = x.wrapping_add(y);
  if r < x || r >= m {
    r.wrapping_sub(m)
  } else {
    r
  }
}

pub fn subm(x: u32, y: u32, m: u32) -> u32 {
  let r = x.wrapping_sub(y);
  if x < y {
    r.wrapping_add(m)
  } else {
    r
  }
}

pub fn mulm(x: u32, y: u32, m: u32) -> u32 {
  (((x as u64) * (y as u64)) % (m as u64)) as u32
}

fn gcdext(a: i32, b: i32) -> (i32, i32, i32) {
  if b == 0 {
    (a, 1, 0)
  } else {
    let (res, y, x) = gcdext(b, a % b);
    (res, x, y - x * (a / b))
  }
}

pub fn inv(n: u32, m: u32) -> u32 {
  let (g, _, x) = gcdext(m as i32, n as i32);
  assert_eq!(g, 1);
  (if x < 0 { x + m as i32 } else { x }) as u32
}

pub struct BinomialsM {
  facts: Vec<u32>,
  ifacts: Vec<u32>,
  modulo: u32,
}

impl BinomialsM {
  pub fn new(maxn: usize, modulo: u32) -> Self {
    let mut ifacts = Vec::with_capacity(maxn + 1);
    ifacts.push(1);
    ifacts.push(1);
    for a in 2..=maxn as u32 {
      let q = modulo / a;
      let r = modulo - a * q;
      ifacts.push(mulm(modulo - q, ifacts[r as usize], modulo));
    }
    let mut facts = Vec::with_capacity(maxn + 1);
    facts.push(1);
    facts.push(1);
    let mut t = 1;
    let mut u = 1;
    for a in 2..=maxn {
      t = mulm(t, a as u32, modulo);
      facts.push(t);
      let w = &mut ifacts[a];
      u = mulm(*w, u, modulo);
      *w = u;
    }
    Self {
      facts,
      ifacts,
      modulo,
    }
  }
  pub fn mul(&self, x: u32, y: u32) -> u32 {
    mulm(x, y, self.modulo)
  }
  pub fn binomial(&self, n: u32, k: u32) -> u32 {
    let n = n as usize;
    let k = k as usize;
    let v1 = self.facts[n];
    let v2 = self.mul(self.ifacts[k], self.ifacts[n - k]);
    self.mul(v1, v2)
  }
}

#[derive(Clone, Copy)]
pub struct IntM(u32, u32);

impl std::ops::Add for IntM {
  type Output = Self;
  fn add(self, other: Self) -> Self {
    debug_assert_eq!(self.0, other.1);
    IntM(addm(self.0, other.0, self.1), self.1)
  }
}

impl std::ops::Sub for IntM {
  type Output = Self;
  fn sub(self, other: Self) -> Self {
    debug_assert_eq!(self.1, other.1);
    IntM(subm(self.0, other.0, self.1), self.1)
  }
}

impl std::ops::Mul for IntM {
  type Output = Self;
  fn mul(self, other: Self) -> Self {
    debug_assert_eq!(self.1, other.1);
    IntM(mulm(self.0, other.0, self.1), self.1)
  }
}
