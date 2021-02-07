use algo::math::sqrtint;
#[test]
fn test_sqrtint() {
  assert_eq!(sqrtint(0), 0);
  assert_eq!(sqrtint(1), 1);
  assert_eq!(sqrtint(2), 1);
  assert_eq!(sqrtint(3), 1);
  assert_eq!(sqrtint(4), 2);
  assert_eq!(sqrtint(9), 3);
  assert_eq!(sqrtint(8), 2);
  assert_eq!(sqrtint(10), 3);
  assert_eq!(sqrtint(25), 5);
  assert_eq!(sqrtint(26), 5);
  assert_eq!(sqrtint(0xffffffffffffffff), 0xffffffff);
  assert_eq!(sqrtint(0xffffffff * 0xffffffff), 0xffffffff);
  assert_eq!(sqrtint(0xffffffff * 0xffffffff - 1), 0xfffffffe);
  assert_eq!(sqrtint(0xfedcba98u64.pow(2)), 0xfedcba98);
  assert_eq!(sqrtint(0xfedcba98u64.pow(2) - 1), 0xfedcba97);
  assert_eq!(sqrtint(117000000), 10816);
}
