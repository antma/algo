use algo::intm::BinomialsM;
#[test]
fn test_binomials() {
  let b = BinomialsM::new(100, 1_000_000_007);
  assert_eq!(b.binomial(100, 50), 538992043);
  assert_eq!(b.binomial(100, 49), 273521609);
  assert_eq!(b.binomial(100, 51), 273521609);
  assert_eq!(b.binomial(100, 0), 1);
}
