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

impl<T: Mul<Output = T> + Clone> Mul<T> for Point<T> {
  type Output = Self;
  fn mul(self, other: T) -> Self::Output {
    Self {
      x: self.x.clone() * other.clone(),
      y: self.y.clone() * other,
    }
  }
}

impl<T: Clone + Add<Output = T> + Mul<Output = T>> Point<T> {
  pub fn dot_product(&self, other: &Self) -> T {
    self.x.clone() * other.x.clone() + self.y.clone() * other.y.clone()
  }
}

//ax + by = c
pub struct Line<T> {
  a: T,
  b: T,
  c: T,
}

impl<T: Clone + Add<Output = T> + Sub<Output = T> + Mul<Output = T>> Line<T> {
  pub fn new(p: &Point<T>, q: &Point<T>) -> Self {
    let a = p.y.clone() - q.y.clone();
    let b = q.x.clone() - p.x.clone();
    Self {
      c: a.clone() * p.x.clone() + b.clone() * p.y.clone(),
      a,
      b,
    }
  }
  pub fn value(&self, p: &Point<T>) -> T {
    self.a.clone() * p.x.clone() + self.b.clone() * p.y.clone() - self.c.clone()
  }
}

impl<T> Line<T>
where
  T: Clone + Add<Output = T> + Sub<Output = T> + Mul<Output = T> + From<i8> + Ord,
{
  fn one_side(&self, p1: &Point<T>, p2: &Point<T>) -> bool {
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
  pub fn intersect_with_line(&self, l: &Line<f64>, eps: f64) -> Vec<Point<f64>> {
    let c0 = l.c - (l.a * self.c.x + l.b * self.c.y);
    let idp = 1.0 / (l.a * l.a + l.b * l.b);
    let e = c0 * idp;
    let x0 = l.a * e;
    let y0 = l.b * e;
    let d = x0.hypot(y0);
    let r = self.r;
    if (r - d).abs() < eps {
      vec![Point { x: x0, y: y0 }]
    } else if r < d {
      Vec::new()
    } else {
      let m = ((r * r - c0 * c0 * idp) * idp).sqrt();
      vec![
        Point {
          x: x0 + l.b * m,
          y: y0 - l.a * m,
        },
        Point {
          x: x0 - l.b * m,
          y: y0 + l.a * m,
        },
      ]
    }
    .into_iter()
    .map(|q| q + self.c)
    .collect()
  }
  pub fn intersect_with_circle(&self, other: &Self, eps: f64) -> Vec<Point<f64>> {
    let p = other.c - self.c;
    let l = Line {
      a: 2.0 * p.x,
      b: 2.0 * p.y,
      c: (p.dot_product(&p) + self.r * self.r) - other.r * other.r,
    };
    let t = Circle {
      c: Point { x: 0.0, y: 0.0 },
      r: self.r,
    };
    t.intersect_with_line(&l, eps)
      .into_iter()
      .map(|q| q + self.c)
      .collect()
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
  T: Add<Output = T> + Sub<Output = T> + Mul<Output = T> + Clone + Ord,
{
  pub fn new(p1: &Point<T>, p2: &Point<T>) -> Self {
    Self {
      p1: p1.clone(),
      p2: p2.clone(),
      l: Line::new(p1, p2),
      pmin: Point {
        x: p1.x.clone().min(p2.x.clone()),
        y: p1.y.clone().min(p2.y.clone()),
      },
      pmax: Point {
        x: p1.x.clone().max(p2.x.clone()),
        y: p1.y.clone().max(p2.y.clone()),
      },
    }
  }
}

impl<T> Segment<T>
where
  T: Clone + Ord + Add<Output = T> + Sub<Output = T> + Mul<Output = T> + From<i8>,
{
  pub fn intersect(&self, other: &Self) -> bool {
    let ux = self.pmin.x.clone().max(other.pmin.x.clone());
    let vx = self.pmax.x.clone().min(other.pmax.x.clone());
    if ux > vx {
      return false;
    }
    let uy = self.pmin.y.clone().max(other.pmin.y.clone());
    let vy = self.pmax.y.clone().min(other.pmax.y.clone());
    if uy > vy {
      return false;
    }
    !(self.l.one_side(&other.p1, &other.p2) || other.l.one_side(&self.p1, &self.p2))
  }
}
