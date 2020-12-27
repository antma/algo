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

#[test]
fn mus() {
  let m2 = vec![
    1, -1, -1, 0, -1, 1, -1, 0, 0, 1, -1, 0, -1, 1, 1, 0, -1, 0, -1, 0, 1, 1, -1, 0, 0, 1, 0, 0,
    -1, -1, -1, 0, 1, 1, 1, 0, -1, 1, 1, 0, -1, -1, -1, 0, 0, 1, -1, 0, 0, 0, 1, 0, -1, 0, 1, 0, 1,
    1, -1, 0, -1, 1, 0, 0, 1, -1, -1, 0, 1, -1, -1, 0, -1, 1, 0, 0, 1, -1,
  ];
  let m1: Vec<_> = algo::linear_sieve::mus(79).into_iter().skip(1).collect();
  assert_eq!(m1, m2);
}
