use crate::geometry::plane::Point;
use std::cmp::Ordering;
use std::ops::{Add, Mul, Sub};

fn left<T>(a: &Point<T>, b: &Point<T>, c: &Point<T>, minimal: bool) -> bool
where
  T: Copy + Ord + Sub<Output = T> + Mul<Output = T>,
{
  let c1 = (c.x - a.x) * (b.y - a.y);
  let c2 = (b.x - a.x) * (c.y - a.y);
  let c = c1.cmp(&c2);
  if minimal {
    c == Ordering::Less
  } else {
    c != Ordering::Greater
  }
}

fn angle_cmp<T>(a: &Point<T>, b: &Point<T>) -> Ordering
where
  T: Clone + Copy + Ord + Mul<Output = T>,
{
  let c1 = a.y * b.x;
  let c2 = b.y * a.x;
  c1.cmp(&c2)
}

pub fn hull<T>(v: &Vec<Point<T>>, minimal: bool) -> Vec<Point<T>>
where
  T:
    Clone + Copy + Ord + PartialEq + From<i8> + Add<Output = T> + Sub<Output = T> + Mul<Output = T>,
{
  if v.len() <= 2 {
    return v.clone();
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
  let mut d = Vec::new();
  for p in q {
    if x.is_empty() || angle_cmp(&x.last().unwrap(), &p) != Ordering::Equal {
      x.push(p);
    } else {
      d.push(p);
    }
  }
  let mut h = Vec::new();
  h.push(Point {
    x: T::from(0),
    y: T::from(0),
  });
  let s = if minimal {
    let s = 2.min(x.len());
    for i in 0..s {
      h.push(x[i]);
    }
    s
  } else {
    let mut i = 0;
    while i < d.len() && angle_cmp(&x[0], &d[i]) == Ordering::Equal {
      i += 1;
    }
    for o in (0..i).rev() {
      h.push(d[o]);
    }
    h.push(x[0]);
    if i == 0 && x.len() > 1 {
      h.push(x[1]);
      2
    } else {
      1
    }
  };
  let mut m = h.len();
  for xi in x.into_iter().skip(s) {
    while !left(&h[m - 2], &h[m - 1], &xi, minimal) {
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
  if !minimal && m > 2 && angle_cmp(&h[1], &h[m - 1]) != Ordering::Equal {
    let mut i = d.len();
    while i > 0 && angle_cmp(&h[m - 1], &d[i - 1]) == Ordering::Equal {
      i -= 1;
    }
    h.extend_from_slice(&d[i..]);
  }
  for p in h.iter_mut() {
    *p = (*p) + me.clone();
  }
  h
}
