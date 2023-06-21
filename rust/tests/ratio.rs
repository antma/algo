use algo::ratio::Ratio;
use std::ops::Neg;
#[test]
fn test_add() {
  let p2 = Ratio::new(1, 2);
  let p3 = Ratio::new(1, 3);
  let p4 = p2 + p3;
  assert_eq!(p4, Ratio::new(5, 6));
  assert_eq!(p2 + p2, Ratio::new(2, 2));
  assert_eq!(
    Ratio::new(123i32, 345i32) + Ratio::new(235i32, 375i32),
    Ratio::new(1696i32, 1725i32)
  );
}

#[test]
fn test_sub() {
  let p2 = Ratio::new(1, 2);
  let p3 = Ratio::new(1, 3);
  assert_eq!(p3 - p2, Ratio::new(-1, 6));
  assert_eq!(p2 - p3, Ratio::new(1, 6));
}

#[test]
fn test_mul() {
  assert_eq!(Ratio::new(63, 6) * Ratio::new(3, 7), Ratio::new(9, 2));
}

#[test]
fn test_neg() {
  assert_eq!(Ratio::new(1i32, 2).neg(), Ratio::new(-1, 2));
}
