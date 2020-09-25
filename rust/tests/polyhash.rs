use algo::polyhash::PolyHash;

#[test]
fn hand1() {
  let base = (256, 127);
  let p1 = PolyHash::new("abab", &base);
  let p2 = PolyHash::new("baa", &base);
  assert_eq!(p1.get(0, 2), p1.get(2, 4));
  assert_eq!(p1.get(1, 3), p2.get(0, 2));
}
