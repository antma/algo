use algo::bignum::bigint::BigInt;
use algo::geometry::plane::{Point, Segment};
use algo::geometry::space;

#[test]
fn test_segments_and_bigints() {
  let zero = BigInt::from(0);
  let one = BigInt::from(1);
  let p = vec![
    Point {
      x: zero.clone(),
      y: zero.clone(),
    },
    Point {
      x: one.clone(),
      y: zero.clone(),
    },
    Point {
      x: one.clone(),
      y: one.clone(),
    },
    Point {
      x: zero.clone(),
      y: one.clone(),
    },
  ];
  let s1 = Segment::new(&p[0], &p[2]);
  let s2 = Segment::new(&p[1], &p[3]);
  //unit square diagonals intersects
  assert_eq!(s1.intersect(&s2), true);
  assert_eq!(p[1].dot_product(&p[3]), zero);
  let mut t = Vec::with_capacity(4);
  for i in 0..4 {
    t.push(Segment::new(&p[i], &p[(i + 1) & 3]));
  }
  //unit square sides intersects, parallel sides does not intersect
  for i in 0..4 {
    let tj = &t[(i + 1) & 3];
    assert_eq!(t[i].intersect(tj), true);
    assert_eq!(tj.intersect(&t[i]), true);
    assert_eq!(t[i].intersect(&t[(i + 2) & 3]), false);
  }
}

#[test]
fn test_segment_contains() {
  let p1 = Point { x: 10i32, y: 15i32 };
  let p2 = p1.clone() * -1;
  let s = Segment::new(&p1, &p2);
  assert!(s.contains(&Point { x: 2, y: 3 }));
  assert!(s.contains(&p1));
  assert!(s.contains(&p2));
}

#[test]
fn geometry_space_bigints() {
  let p1 = space::Point {
    x: BigInt::from(1),
    y: BigInt::from(2),
    z: BigInt::from(3),
  };
  let p2 = space::Point {
    x: BigInt::from(4),
    y: BigInt::from(5),
    z: BigInt::from(6),
  };
  let p3 = space::Point {
    x: BigInt::from(5),
    y: BigInt::from(7),
    z: BigInt::from(9),
  };
  let p4 = space::Point {
    x: BigInt::from(-3),
    y: BigInt::from(-3),
    z: BigInt::from(-3),
  };
  let p5 = space::Point {
    x: BigInt::from(10),
    y: BigInt::from(14),
    z: BigInt::from(18),
  };
  assert_eq!(p1.clone() + p2.clone(), p3);
  assert_eq!(p1.clone() - p2.clone(), p4);
  assert_eq!(p3.clone() * BigInt::from(2), p5);
  assert_eq!(p1.dot_product(&p2), BigInt::from(4 + 2 * 5 + 3 * 6));
}

#[test]
fn test_space_cross_product() {
  let a: space::Point<i32> = space::Point { x: 2, y: -3, z: -1 };
  let b: space::Point<i32> = space::Point { x: 3, y: -1, z: -4 };
  let c: space::Point<i32> = space::Point { x: 11, y: 5, z: 7 };
  assert_eq!(a.cross_product(&b), c);
  assert_eq!(b.cross_product(&a), c * -1);
}
