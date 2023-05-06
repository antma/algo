use std::ops::{Add, Div, Mul};
#[derive(Clone)]
pub struct UBigF(pub f64, pub i32);

impl UBigF {
  fn norm(self) -> Self {
    let mut u = self.0;
    let mut v = self.1;
    while u > 1.0 {
      u *= 0.5;
      v += 1;
    }
    while u < 0.5 {
      u *= 2.0;
      v -= 1;
    }
    Self(u, v)
  }
  pub fn get(&self) -> f64 {
    self.0 * 2f64.powi(self.1)
  }
}

impl Add for UBigF {
  type Output = Self;
  fn add(self, other: Self) -> Self {
    let x = self.1 - other.1;
    if x > 64 {
      return self;
    }
    if x < -64 {
      return other;
    }
    let (u, v, x) = if x >= 0 {
      (&self, &other, x)
    } else {
      (&other, &self, -x)
    };
    Self(u.0 + 0.5f64.powi(x) * v.0, u.1).norm()
  }
}

impl Mul for UBigF {
  type Output = Self;
  fn mul(self, other: Self) -> Self {
    Self(self.0 * other.0, self.1 + other.1).norm()
  }
}

impl Div for UBigF {
  type Output = Self;
  fn div(self, other: Self) -> Self {
    Self(self.0 / other.0, self.1 - other.1).norm()
  }
}
