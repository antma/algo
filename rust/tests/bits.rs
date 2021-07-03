use algo::bits::Submasks;

#[test]
fn bits_submasks_hand_tests() {
  let mut a = Submasks(6).into_iter().collect::<Vec<_>>();
  a.sort();
  assert_eq!(a, vec![2, 4, 6]);
}
