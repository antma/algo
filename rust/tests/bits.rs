use algo::bits::GrayCodes;
use algo::bits::Submasks;

#[test]
fn bits_submasks_hand_tests() {
  let mut a = Submasks(6).into_iter().collect::<Vec<_>>();
  a.sort();
  assert_eq!(a, vec![2, 4, 6]);
}

#[test]
fn bits_graycodes_tests() {
  assert_eq!(GrayCodes(1).into_iter().collect::<Vec<_>>(), vec![0, 1]);
  assert_eq!(
    GrayCodes(2).into_iter().collect::<Vec<_>>(),
    vec![0, 1, 3, 2]
  );
}
