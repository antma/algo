use algo::number_theory::binomialsm::BinomialsM;
use algo::number_theory::miller::PrimalityTest32;
use algo::primes::PrimeTable;

#[test]
fn number_theory_binomialsm_tests() {
  let b = BinomialsM::new(100, 1_000_000_007);
  assert_eq!(b.binomial(100, 50), 538992043);
  assert_eq!(b.binomial(100, 49), 273521609);
  assert_eq!(b.binomial(100, 51), 273521609);
  assert_eq!(b.binomial(100, 0), 1);
}

#[test]
fn number_theory_miller_tests() {
  const M: u32 = 1_000_000;
  let pt = PrimeTable::new(M as usize);
  let mut miller = PrimalityTest32::new();
  for p in 1..M {
    assert_eq!(miller.is_prime(p), pt.is_prime(p), "p = {}", p);
  }
}
