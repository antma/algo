use algo::geometry::convex_hull::*;
use algo::geometry::plane::Point;

type P = Point<i64>;
#[test]
fn hand1() {
  let p = vec![P { x: 1, y: 1 }, P { x: 2, y: 2 }];
  let h = hull(&p);
  assert_eq!(p, h);
  let q = vec![P { x: 1, y: 1 }, P { x: 2, y: 2 }, P { x: 3, y: 3 }];
  let h = hull(&q);
  assert_eq!(h, vec![P { x: 1, y: 1 }, P { x: 3, y: 3 }]);
  let q = vec![
    P { x: 200, y: 400 },
    P { x: 300, y: 400 },
    P { x: 300, y: 300 },
    P { x: 400, y: 300 },
    P { x: 400, y: 400 },
    P { x: 500, y: 400 },
    P { x: 500, y: 200 },
    P { x: 350, y: 200 },
    P { x: 200, y: 200 },
  ];
  assert_eq!(
    hull(&q),
    vec![
      P { x: 200, y: 200 },
      P { x: 500, y: 200 },
      P { x: 500, y: 400 },
      P { x: 200, y: 400 }
    ]
  );
}
