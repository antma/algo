use algo::exponentation::pow;

#[test]
fn exponentation_tests() {
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
