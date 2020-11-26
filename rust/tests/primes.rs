use algo::primes::PrimeTable;

#[test]
fn large() {
  let pt = PrimeTable::new(2000000);
  let large_primes = pt.primes();
  assert_eq!(
    large_primes
      .iter()
      .take_while(|&&i| i < 1_000_000)
      .map(|&i| i as u64)
      .sum::<u64>(),
    37550402023
  );
  assert_eq!(
    large_primes.iter().map(|&i| i as u64).sum::<u64>(),
    142913828922
  );
  for i in 1..100 {
    let l1 = large_primes
      .iter()
      .take_while(|&&j| j < i)
      .cloned()
      .collect::<Vec<_>>();
    let pt2 = PrimeTable::new(i as usize);
    let l2 = pt2.primes();
    assert_eq!(l1, l2);
  }
}
