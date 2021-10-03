use algo::fenwick_tree::FenwickTree;
use algo::number_theory::intm_fixed::IntM;

#[test]
fn fenwick_tree_intm_tests() {
  let minus_one = IntM(0) - IntM(1);
  let mut ft = FenwickTree::<IntM>::new(5);
  ft.update(2, IntM(1));
  assert_eq!(ft.reduce(3), IntM(1));
  ft.update(0, minus_one);
  ft.update(1, minus_one);
  assert_eq!(ft.reduce(3), minus_one);
}
