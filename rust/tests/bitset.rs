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

#[test]
fn bitset_bits_set_range() {
  let mut bs = BitSet::new(300);
  bs.set_range(60..100);
  assert_eq!(bs.bits().collect::<Vec<_>>(), (60..100).collect::<Vec<_>>());
  let mut bs = BitSet::new(300);
  bs.set_range(60..63);
  assert_eq!(bs.bits().collect::<Vec<_>>(), (60..63).collect::<Vec<_>>());
  let mut bs = BitSet::new(300);
  bs.set_range(0..0);
  assert_eq!(bs.bits().collect::<Vec<_>>(), Vec::new());
  bs.clear_all();
  bs.set(1);
  bs.set(125);
  bs.set_range(62..67);
  assert_eq!(
    bs.bits().collect::<Vec<_>>(),
    vec![1, 62, 63, 64, 65, 66, 125]
  );
}
