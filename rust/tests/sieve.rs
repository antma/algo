#[test]
fn overflow() {
  let sv = algo::sieve::Sieve::new(368871 + 1);
  let f = sv.factorization(368871);
  assert_eq!(f.divisors(), vec![1, 3, 122957, 368871]);
}
