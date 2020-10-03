use algo::splay_tree::SplayTree;
use std::collections::HashSet;

#[test]
fn insert() {
  let mut s = HashSet::<u32>::new();
  let mut u = 1u32;
  let mut t = SplayTree::<u32>::new();
  const M: usize = 100;
  for i in 0..M {
    u = u.wrapping_mul(239);
    s.insert(u);
    t.insert(u);
    assert!(t.contains(&u));
    assert_eq!(t.count(), i + 1);
  }
  for v in s.iter() {
    assert!(t.contains(v));
    t.remove(v);
    assert!(!t.contains(v));
  }
}
