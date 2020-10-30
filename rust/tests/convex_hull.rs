use algo::geometry::convex_hull::*;
use algo::geometry::plane::Point;

type P = Point<i64>;
#[test]
fn hand_minimal() {
  let p = vec![P { x: 1, y: 1 }, P { x: 2, y: 2 }];
  let h = hull(&p, true);
  assert_eq!(p, h);
  let q = vec![P { x: 1, y: 1 }, P { x: 2, y: 2 }, P { x: 3, y: 3 }];
  let h = hull(&q, true);
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
    hull(&q, true),
    vec![
      P { x: 200, y: 200 },
      P { x: 500, y: 200 },
      P { x: 500, y: 400 },
      P { x: 200, y: 400 }
    ]
  );
  let w = vec![P { x: 1, y: 2 }];
  assert_eq!(hull(&w, true), w);
}
#[test]
fn hand_not_minimal() {
  let q = vec![
    P { x: 100, y: 100 },
    P { x: 200, y: 100 },
    P { x: 100, y: 200 },
    P { x: 300, y: 300 },
  ];
  assert_eq!(
    hull(&q, false),
    vec![
      P { x: 100, y: 100 },
      P { x: 200, y: 100 },
      P { x: 300, y: 300 },
      P { x: 100, y: 200 }
    ]
  );
}
#[test]
fn square() {
  const M: i64 = 1;
  let mut q = Vec::new();
  for x in -M..=M {
    for y in -M..=M {
      q.push(P { x, y });
    }
  }
  assert_eq!(
    hull(&q, true),
    vec![
      P { x: -M, y: -M },
      P { x: M, y: -M },
      P { x: M, y: M },
      P { x: -M, y: M },
    ]
  );
  let mut w = Vec::new();
  for x in -M..=M {
    w.push(P { x, y: -M });
  }
  for y in -M + 1..=M {
    w.push(P { x: M, y });
  }
  for x in (-M..=M - 1).rev() {
    w.push(P { x, y: M });
  }
  for y in (-M + 1..=M - 1).rev() {
    w.push(P { x: -M, y });
  }
  assert_eq!(hull(&q, false), w);
}
