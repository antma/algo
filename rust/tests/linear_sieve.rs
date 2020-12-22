#[test]
fn totients() {
  let phi = algo::linear_sieve::totients(1001);
  assert_eq!(phi.len(), 1001);
  assert_eq!(
    phi
      .into_iter()
      .enumerate()
      .skip(1)
      .map(|(i, x)| i as i32 - x)
      .sum::<i32>(),
    196308
  );
}
