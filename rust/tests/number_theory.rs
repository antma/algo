use algo::number_theory::binomialsm::BinomialsM;
use algo::number_theory::exponentation::pow;
use algo::number_theory::miller::PrimalityTest32;
use algo::number_theory::miller::PrimalityTest64;
use algo::primes::PrimeTable;
use algo::random::KnuthRandom;

#[test]
fn number_theory_exponentation_tests() {
  for i in 0..32 {
    assert_eq!(pow(2u32, i, || 1u32), 1u32 << i);
  }
  for i in 0..64 {
    assert_eq!(pow(2u64, i, || 1u64), 1u64 << i);
  }
  for i in 0..=40 {
    assert_eq!(pow(3u64, i, || 1u64), 3u64.pow(i));
  }
}

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
  let mut rnd = KnuthRandom::new(12345);
  let mut miller64 = PrimalityTest64::new();
  let off = 10u64.pow(16);
  let big_primes = vec![61, 69, 79, 99, 453];
  let mut last = 0;
  for p in &big_primes {
    assert_eq!(
      miller64.is_prime(&mut rnd, off + *p, 10),
      true,
      "{} should be prime",
      off + *p
    );
    for j in last + 1..*p {
      assert_eq!(
        miller64.is_prime(&mut rnd, off + j, 10),
        false,
        "{} should be composite",
        off + j
      );
    }
    last = *p;
  }
}
