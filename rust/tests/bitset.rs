use algo::bitset::BitSet;

#[test]
fn bitset_bits_tests() {
  let mut bs = BitSet::new(1000);
  let v = vec![1, 3, 5, 7, 30, 239, 777];
  for p in &v {
    bs.set(*p);
  }
  assert_eq!(bs.bits().collect::<Vec<_>>(), v);
}

#[test]
fn bitset_bits_empty_test() {
  let bs = BitSet::new(0);
  assert_eq!(bs.bits().collect::<Vec<_>>(), Vec::new());
}
