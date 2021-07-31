use algo::number_theory;
use number_theory::binomialsm::BinomialsM;
use number_theory::exponentation::pow;
use number_theory::factorization::factorization64;
use number_theory::gcd::Gcd;
use number_theory::primality_test32::is_prime32;
use number_theory::primality_test64::is_prime64;
use number_theory::primes::PrimeTable;
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
fn number_theory_gcd_tests() {
  let mut g = Gcd::new();
  let mut x = 1;
  let mut y = 2;
  for _ in 0..44 {
    assert_eq!(g.gcd_u32(x, y), 1);
    assert_eq!(g.gcd_u32(y, x), 1);
    let z = x + y;
    x = y;
    y = z;
  }
  assert_eq!(g.gcd_u32(1, 0), 1);
  assert_eq!(g.gcd_u32(0, 1), 1);
  assert_eq!(g.gcd_u32(1 << 30, 2), 2);
  assert_eq!(g.gcd_u32(0xffff_ffff, 239), 1);
  assert_eq!(g.gcd_u32(255, 255), 255);
  assert_eq!(g.gcd_u32(255, 254), 1);
  assert_eq!(g.gcd_u64(123456789012345678, 2345678901234567890), 2);
}

#[test]
fn number_theory_miller_carmichael_numbers_tests() {
  let mut gcd = Gcd::new();
  for x in [
    561, 1105, 1729, 2465, 2821, 6601, 8911, 10585, 15841, 29341, 41041, 46657, 52633, 62745,
    63973, 75361, 101101, 115921, 126217, 162401, 172081, 188461, 252601, 278545, 294409, 314821,
    334153, 340561, 399001, 410041, 449065, 488881, 512461,
  ]
  .iter()
  {
    assert!(!is_prime32(*x, &mut gcd));
  }
  let mut rnd = KnuthRandom::new(12345);
  for x in [
    825265,
    321197185,
    5394826801,
    232250619601,
    9746347772161,
    1436697831295441,
    60977817398996785,
    7156857700403137441,
  ]
  .iter()
  {
    assert!(!is_prime64(*x, &mut gcd, &mut rnd, 10));
  }
}

#[test]
fn number_theory_miller_tests() {
  let mut gcd = Gcd::new();
  const M: u32 = 1_000_000;
  let pt = PrimeTable::new(M as usize);
  for p in 1..M {
    assert_eq!(is_prime32(p, &mut gcd), pt.is_prime(p), "p = {}", p);
  }
  let mut rnd = KnuthRandom::new(12345);
  let mut check = |off: u64, big_primes: Vec<u64>| {
    let mut last = 0;
    for p in big_primes {
      assert_eq!(
        is_prime64(off + p, &mut gcd, &mut rnd, 10),
        true,
        "{} should be prime",
        off + p
      );
      for j in last + 1..p {
        assert_eq!(
          is_prime64(off + j, &mut gcd, &mut rnd, 10),
          false,
          "{} should be composite",
          off + j
        );
      }
      last = p;
    }
  };
  check(
    4 * 10u64.pow(9),
    vec![
      7, 9, 19, 63, 133, 157, 163, 187, 229, 231, 241, 273, 351, 373, 387, 427, 451, 453, 483, 531,
      553, 559, 561, 573, 579, 591, 597, 607, 619, 661, 663, 687, 723, 727, 733, 787, 801, 813,
      831, 861, 913, 951, 981, 1003,
    ],
  );
  check(10u64.pow(16), vec![61, 69, 79, 99, 453]);
}

#[test]
fn number_theory_factorization_test() {
  let mut gcd = Gcd::new();
  let mut rnd = KnuthRandom::new(123);
  let small_primes = PrimeTable::new(1000).primes();
  assert_eq!(
    factorization64(1u64 << 62, &small_primes, &mut gcd, &mut rnd, 10).0,
    vec![(2, 62)]
  );
  assert_eq!(
    factorization64(1024384027, &small_primes, &mut gcd, &mut rnd, 10).0,
    vec![(32003, 1), (32009, 1)]
  );
  //32783362016081: 32003 32003 32009
  assert_eq!(
    factorization64(32783362016081, &small_primes, &mut gcd, &mut rnd, 10).0,
    vec![(32003, 2), (32009, 1)]
  );
  assert_eq!(
    factorization64(997, &small_primes, &mut gcd, &mut rnd, 10).0,
    vec![(997, 1)]
  );
  assert_eq!(
    factorization64(2286236833561957, &small_primes, &mut gcd, &mut rnd, 10).0,
    vec![(21491747, 1), (106377431, 1)]
  );
  //123456789012345678: 2 3 3 3 21491747 106377431
  assert_eq!(
    factorization64(123456789012345678, &small_primes, &mut gcd, &mut rnd, 10).0,
    vec![(2, 1), (3, 3), (21491747, 1), (106377431, 1)]
  );
  assert_eq!(
    factorization64(0x7fffffffffffffff, &small_primes, &mut gcd, &mut rnd, 3).0,
    vec![(7, 2), (73, 1), (127, 1), (337, 1), (92737, 1), (649657, 1)]
  );
  assert_eq!(
    factorization64(998864158694426617, &small_primes, &mut gcd, &mut rnd, 3).0,
    vec![(999055237, 1), (999808741, 1)]
  );
}

#[test]
fn number_theory_sieve_array_overflow() {
  let sv = algo::number_theory::sieve_array::SieveArray::new(368871 + 1);
  let f = sv.factorization(368871);
  assert_eq!(f.divisors(), vec![1, 3, 122957, 368871]);
}
