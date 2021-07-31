use algo::bignum::bigint::BigInt;
use algo::geometry::plane::{Point, Segment};

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
