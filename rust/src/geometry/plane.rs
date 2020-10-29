use std::ops::{Add, Mul, Sub};

#[derive(Debug, Copy)]
pub struct Point<T> {
  pub x: T,
  pub y: T,
}

impl<T: Clone> Clone for Point<T> {
  fn clone(&self) -> Self {
    Self {
      x: self.x.clone(),
      y: self.y.clone(),
    }
  }
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

impl<T: PartialEq> PartialEq for Point<T> {
  fn eq(&self, other: &Point<T>) -> bool {
    self.x == other.x && self.y == other.y
  }
}

impl Point<f64> {
  pub fn dist(&self, other: &Self) -> f64 {
    let d = *self - *other;
    d.x.hypot(d.y)
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

impl<T: Copy + Add<Output = T> + Sub<Output = T> + Mul<Output = T>> Point<T> {
  fn dot_product(&self, other: &Self) -> T {
    self.x * other.x + self.y * other.y
  }
}

//ax + by = c
pub struct Line<T> {
  a: T,
  b: T,
  c: T,
}

impl<T: Copy + Add<Output = T> + Sub<Output = T> + Mul<Output = T>> Line<T> {
  pub fn new(p: &Point<T>, q: &Point<T>) -> Self {
    let a = p.y - q.y;
    let b = q.x - p.x;
    Self {
      a,
      b,
      c: a * p.x + b * p.y,
    }
  }
}

impl Line<f64> {
  pub fn intersect(&self, other: &Self, eps: f64) -> Option<Point<f64>> {
    let d = self.a * other.b - other.a * self.b;
    if d.abs() < eps {
      None
    } else {
      Some(
        Point {
          x: self.c - other.b - other.c * self.b,
          y: self.a * other.c - other.a * self.c,
        } * (1.0 / d),
      )
    }
  }
  pub fn bisection(p: &Point<f64>, q: &Point<f64>) -> Self {
    let m = (*p + *q) * 0.5;
    let d = *q - *p;
    Self {
      a: d.x,
      b: d.y,
      c: d.dot_product(&m),
    }
  }
}

pub struct Circle<T> {
  pub c: Point<T>,
  pub r: T,
}

impl Circle<f64> {
  pub fn circumcircle(p1: &Point<f64>, p2: &Point<f64>, p3: &Point<f64>, eps: f64) -> Option<Self> {
    let l1 = Line::bisection(p1, p2);
    let l2 = Line::bisection(p2, p3);
    l1.intersect(&l2, eps).map(|p| Circle {
      c: p,
      r: p.dist(p1),
    })
  }
}
