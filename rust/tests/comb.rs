use algo::comb::permutations_heap::PermutationsHeap;
use std::collections::HashSet;

fn permutations_heap_check(n: usize) {
  let mut a = (1..=n).collect::<Vec<_>>();
  let mut p = PermutationsHeap::new(&a);
  let mut h = HashSet::new();
  h.insert(a.clone());
  let m = (1..=n).into_iter().product();
  for _ in 1..m {
    assert!(p.next_permutation(&mut a));
    h.insert(a.clone());
  }
  assert!(!p.next_permutation(&mut a));
  assert_eq!(h.len(), m);
  assert!(h.into_iter().all(|q| (1..=n).all(|i| q.contains(&i))));
}

#[test]
fn comb_permutation_heap_tests() {
  for n in 1..=5 {
    permutations_heap_check(n);
  }
}
