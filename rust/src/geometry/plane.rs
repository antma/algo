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
  pub fn value(&self, p: &Point<T>) -> T {
    self.a * p.x + self.b * p.y - self.c
  }
}

impl<T> Line<T>
where
  T: Copy + Add<Output = T> + Sub<Output = T> + Mul<Output = T> + From<i8> + Ord,
{
  pub fn one_side(&self, p1: &Point<T>, p2: &Point<T>) -> bool {
    let z = T::from(0);
    let v1 = self.value(p1);
    let v2 = self.value(p2);
    (v1 > z && v2 > z) || (v1 < z && v2 < z)
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
          x: self.c * other.b - other.c * self.b,
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

pub struct Segment<T> {
  p1: Point<T>,
  p2: Point<T>,
  l: Line<T>,
  pmin: Point<T>,
  pmax: Point<T>,
}

impl<T> Segment<T>
where
  T: Add<Output = T> + Sub<Output = T> + Mul<Output = T> + Copy + Ord,
{
  pub fn new(p1: &Point<T>, p2: &Point<T>) -> Self {
    Self {
      p1: *p1,
      p2: *p2,
      l: Line::new(p1, p2),
      pmin: Point {
        x: p1.x.min(p2.x),
        y: p1.y.min(p2.y),
      },
      pmax: Point {
        x: p1.x.max(p2.x),
        y: p1.y.max(p2.y),
      },
    }
  }
}

impl<T> Segment<T>
where
  T: Copy + Ord + Add<Output = T> + Sub<Output = T> + Mul<Output = T> + From<i8>,
{
  pub fn intersect(&self, other: &Self) -> bool {
    let ux = self.pmin.x.max(other.pmin.x);
    let vx = self.pmax.x.min(other.pmax.x);
    if ux > vx {
      return false;
    }
    let uy = self.pmin.y.max(other.pmin.y);
    let vy = self.pmax.y.min(other.pmax.y);
    if uy > vy {
      return false;
    }
    !(self.l.one_side(&other.p1, &other.p2) || other.l.one_side(&self.p1, &self.p2))
  }
}
