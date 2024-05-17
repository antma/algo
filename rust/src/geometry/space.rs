use std::ops::{Add, Mul, Sub};

#[derive(Debug, Copy)]
pub struct Point<T> {
  pub x: T,
  pub y: T,
  pub z: T,
}

impl<T: Clone> Clone for Point<T> {
  fn clone(&self) -> Self {
    Self {
      x: self.x.clone(),
      y: self.y.clone(),
      z: self.z.clone(),
    }
  }
}

impl<T: Add<Output = T>> Add for Point<T> {
  type Output = Self;
  fn add(self, other: Point<T>) -> Point<T> {
    Self {
      x: self.x + other.x,
      y: self.y + other.y,
      z: self.z + other.z,
    }
  }
}

impl<T: Sub<Output = T>> Sub for Point<T> {
  type Output = Self;
  fn sub(self, other: Point<T>) -> Point<T> {
    Self {
      x: self.x - other.x,
      y: self.y - other.y,
      z: self.z - other.z,
    }
  }
}

impl<T: Clone + Add<Output = T> + Mul<Output = T>> Point<T> {
  pub fn dot_product(&self, other: &Self) -> T {
    self.x.clone() * other.x.clone()
      + self.y.clone() * other.y.clone()
      + self.z.clone() * other.z.clone()
  }
}

fn det2<T: Sub<Output = T> + Mul<Output = T>>(a: T, b: T, c: T, d: T) -> T {
  a * d - b * c
}

impl<T: Clone + Sub<Output = T> + Mul<Output = T>> Point<T> {
  pub fn cross_product(&self, other: &Self) -> Self {
    Point {
      x: det2(
        self.y.clone(),
        self.z.clone(),
        other.y.clone(),
        other.z.clone(),
      ),
      y: det2(
        self.z.clone(),
        self.x.clone(),
        other.z.clone(),
        other.x.clone(),
      ),
      z: det2(
        self.x.clone(),
        self.y.clone(),
        other.x.clone(),
        other.y.clone(),
      ),
    }
  }
}

impl<T: Mul<Output = T> + Clone> Mul<T> for Point<T> {
  type Output = Self;
  fn mul(self, other: T) -> Self {
    Self {
      x: self.x * other.clone(),
      y: self.y * other.clone(),
      z: self.z * other,
    }
  }
}

impl<T: PartialEq> PartialEq for Point<T> {
  fn eq(&self, other: &Point<T>) -> bool {
    self.x == other.x && self.y == other.y && self.z == other.z
  }
}

impl Point<f64> {
  pub fn dist(&self, other: &Self) -> f64 {
    let d = *self - *other;
    (d.dot_product(&d)).sqrt()
  }
}
