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

pub fn powm<P>(x: u32, mut y: P, m: u32) -> u32
where
  P: From<bool> + Eq + std::ops::BitAnd<Output = P> + std::ops::ShrAssign<u8> + Copy,
{
  let zero = P::from(false);
  if y == zero {
    return 1;
  }
  let mut b = x;
  let one = P::from(true);
  while zero == (y & one) {
    b = mulm(b, b, m);
    y >>= 1;
  }
  let mut a = b;
  y >>= 1;
  while y != zero {
    b = mulm(b, b, m);
    if !((y & one) == zero) {
      a = mulm(a, b, m);
    }
    y >>= 1;
  }
  a
}

#[derive(Clone, Copy)]
pub struct IntM(u32, u32);

impl std::ops::Add for IntM {
  type Output = Self;
  fn add(self, other: Self) -> Self {
    debug_assert_eq!(self.1, other.1);
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
