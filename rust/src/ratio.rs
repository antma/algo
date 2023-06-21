use std::ops::{Add, Div, Mul, Neg, Rem, Sub};

#[derive(Clone, Copy, Debug, Eq)]
pub struct Ratio<T> {
  pub num: T,
  pub den: T,
}

fn gcd<T>(a: T, b: T) -> T
where
  T: Copy + PartialEq + From<i8> + Rem<Output = T>,
{
  if b == T::from(0) {
    a
  } else {
    gcd(b, a % b)
  }
}

fn agcd<T>(a: T, b: T) -> T
where
  T: Copy + PartialOrd + From<i8> + Rem<Output = T> + Neg<Output = T>,
{
  let aa = if a < T::from(0) { a.neg() } else { a };
  gcd(aa, b)
}

impl<T: PartialEq> PartialEq for Ratio<T> {
  fn eq(&self, other: &Ratio<T>) -> bool {
    self.num == other.num && self.den == other.den
  }
}

impl<T> Ratio<T>
where
  T: Copy + PartialOrd + From<i8> + Rem<Output = T> + Neg<Output = T> + Div<Output = T>,
{
  pub fn new(a: T, b: T) -> Self {
    assert!(b > T::from(0));
    let g = agcd(a, b);
    if g > T::from(1) {
      Self {
        num: a / g,
        den: b / g,
      }
    } else {
      Self { num: a, den: b }
    }
  }
}

impl<T> Add for Ratio<T>
where
  T: Copy
    + PartialEq
    + From<i8>
    + Rem<Output = T>
    + Neg<Output = T>
    + PartialOrd
    + Mul<Output = T>
    + Div<Output = T>
    + Add<Output = T>,
{
  type Output = Self;
  fn add(self, other: Ratio<T>) -> Self {
    let g = gcd(self.den, other.den);
    if g > T::from(1) {
      let t = other.den / g;
      Ratio::new(self.num * t + other.num * (self.den / g), self.den * t)
    } else {
      Ratio::new(
        self.num * other.den + other.num * self.den,
        self.den * other.den,
      )
    }
  }
}

impl<T> Sub for Ratio<T>
where
  T: Copy
    + PartialEq
    + From<i8>
    + Rem<Output = T>
    + Neg<Output = T>
    + PartialOrd
    + Mul<Output = T>
    + Div<Output = T>
    + Sub<Output = T>,
{
  type Output = Self;
  fn sub(self, other: Ratio<T>) -> Self {
    let g = gcd(self.den, other.den);
    if g > T::from(1) {
      let t = other.den / g;
      Ratio::new(self.num * t - other.num * (self.den / g), self.den * t)
    } else {
      Ratio::new(
        self.num * other.den - other.num * self.den,
        self.den * other.den,
      )
    }
  }
}

impl<T> Mul for Ratio<T>
where
  T: Copy
    + PartialEq
    + From<i8>
    + Rem<Output = T>
    + Neg<Output = T>
    + PartialOrd
    + Mul<Output = T>
    + Div<Output = T>,
{
  type Output = Self;
  fn mul(self, other: Ratio<T>) -> Self {
    let g1 = agcd(self.num, other.den);
    let g2 = agcd(other.num, self.den);
    Self {
      num: (self.num / g1) * (other.num / g2),
      den: (other.den / g1) * (self.den / g2),
    }
  }
}

impl<T> Ord for Ratio<T>
where
  T: Copy + Mul<Output = T> + Ord,
{
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    let ad = self.num * other.den;
    let bc = self.den * other.num;
    ad.cmp(&bc)
  }
}

impl<T> PartialOrd for Ratio<T>
where
  T: Copy + Mul<Output = T> + Ord,
{
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl<T> From<i8> for Ratio<T>
where
  T: Copy + PartialOrd + From<i8> + Rem<Output = T> + Neg<Output = T> + Div<Output = T>,
{
  fn from(x: i8) -> Self {
    Self::new(T::from(x), T::from(1))
  }
}

impl<T> Neg for Ratio<T>
where
  T: Neg<Output = T>,
{
  type Output = Self;
  fn neg(self) -> Self::Output {
    Ratio {
      num: self.num.neg(),
      den: self.den,
    }
  }
}
