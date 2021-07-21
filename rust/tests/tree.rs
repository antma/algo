use algo::tree::treap;

#[derive(Default)]
struct Void;

type TNode = treap::Node<i32, Void, Void>;
fn nop(_: &mut TNode) {}

#[test]
fn treap_tests() {
  let t = treap::Treap { relax_op: nop };
  let mut root = None;
  let mut rnd = algo::random::KnuthRandom::new(12345);
  for p in vec![2, 3, 5, 7, 11, 13, 17, 19] {
    root = t.insert(root, p, rnd.randrange(0..1_000_000_000), Void::default());
  }
  for (key, res) in vec![
    (1, 0),
    (2, 0),
    (3, 1),
    (4, 2),
    (5, 2),
    (6, 3),
    (7, 3),
    (8, 4),
    (9, 4),
    (10, 4),
    (11, 4),
    (12, 5),
  ] {
    assert_eq!(treap::Treap::count_less(&root, &key), res);
  }
  let (root, b) = t.remove(root, &5);
  assert!(b);
  for (key, res) in vec![
    (1, 0),
    (2, 0),
    (3, 1),
    (4, 2),
    (5, 2),
    (6, 2),
    (7, 2),
    (8, 3),
    (9, 3),
    (10, 3),
    (11, 3),
    (12, 4),
  ] {
    assert_eq!(treap::Treap::count_less(&root, &key), res);
  }
}
