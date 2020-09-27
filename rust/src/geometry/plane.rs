use std::ops::{Add, Mul, Sub};

#[derive(Debug, Copy, Clone)]
pub struct Point<T> {
  x: T,
  y: T,
}

impl<T: Add<Output = T>> Add for Point<T> {
  type Output = Self;
  fn add(self, other: Point<T>) -> Point<T> {
    Self {
      x: self.x + other.x,
      y: self.y + other.y,
    }
  }
}

impl<T: Sub<Output = T>> Sub for Point<T> {
  type Output = Self;
  fn sub(self, other: Point<T>) -> Point<T> {
    Self {
      x: self.x - other.x,
      y: self.y - other.y,
    }
  }
}

impl<T: Mul<Output = T> + Copy> Mul<T> for Point<T> {
  type Output = Self;
  fn mul(self, other: T) -> Self::Output {
    Self {
      x: self.x * other,
      y: self.y * other,
    }
  }
}
