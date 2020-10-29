use crate::geometry::plane::Point;
use std::cmp::Ordering;
use std::ops::{Add, Mul, Sub};

fn left<T>(a: &Point<T>, b: &Point<T>, c: &Point<T>) -> bool
where
  T: Copy + Ord + Sub<Output = T> + Mul<Output = T>,
{
  let c1 = (c.x - a.x) * (b.y - a.y);
  let c2 = (b.x - a.x) * (c.y - a.y);
  c1.cmp(&c2) == Ordering::Less
}

fn angle_cmp<T>(a: &Point<T>, b: &Point<T>) -> Ordering
where
  T: Clone + Copy + Ord + Mul<Output = T>,
{
  let c1 = a.y * b.x;
  let c2 = b.y * a.x;
  c1.cmp(&c2)
}

pub fn hull<T>(v: &Vec<Point<T>>) -> Vec<Point<T>>
where
  T: Clone
    + Copy
    + Ord
    + PartialEq
    + From<i32>
    + Add<Output = T>
    + Sub<Output = T>
    + Mul<Output = T>,
{
  if v.is_empty() {
    return Vec::new();
  }
  let me = v
    .iter()
    .min_by(|p, q| {
      if p.y != q.y {
        p.y.cmp(&q.y)
      } else {
        p.x.cmp(&q.x)
      }
    })
    .unwrap();
  let mut q: Vec<Point<T>> = Vec::new();
  for p in v.iter() {
    if *p != *me {
      q.push(p.clone() - me.clone());
    }
  }
  q.sort_by(|a, b| {
    let c = angle_cmp(a, b);
    if c == Ordering::Equal {
      let d1 = a.x * a.x + a.y * a.y;
      let d2 = b.x * b.x + b.y * b.y;
      d2.cmp(&d1)
    } else {
      c
    }
  });
  let mut x = Vec::new();
  for p in q {
    if x.is_empty() || angle_cmp(&x.last().unwrap(), &p) != Ordering::Equal {
      x.push(p);
    }
  }
  let mut h = Vec::new();
  h.push(Point {
    x: T::from(0),
    y: T::from(0),
  });
  for i in 0..2.min(x.len()) {
    h.push(x[i]);
  }
  let mut m = h.len();
  for xi in x.into_iter().skip(2) {
    while !left(&h[m - 2], &h[m - 1], &xi) {
      m -= 1;
    }
    if m < h.len() {
      h[m] = xi;
    } else {
      h.push(xi);
    }
    m += 1;
  }
  h.truncate(m);
  for p in h.iter_mut() {
    *p = (*p) + me.clone();
  }
  h
}
