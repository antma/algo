use algo::random::KnuthRandom;
use algo::tree::treap;
use algo::tree::treap_implicit_key;
use algo::tree::treap_persistent_implicit_key;

#[derive(Clone, Default)]
struct Void;

type TNode = treap::Node<i32, Void, Void>;
fn nop(_: &mut TNode) {}

type INode = treap_implicit_key::INode<i8, Void>;
type PtrPINode = treap_persistent_implicit_key::PtrPINode<i8, Void>;

fn pi_nop(_l: &PtrPINode, _r: &PtrPINode, _v: &i8) -> Void {
  Void
}

fn i_nop(_l: &mut INode) {}

#[test]
fn treap_implicit_key_tests() {
  const M: usize = 100;
  let mut rnd = KnuthRandom::new(12345);
  let t = treap_implicit_key::ImplicitKeyTreap::new(i_nop, i_nop);
  let mut root = None;
  for i in 0..M {
    root = t.insert(root, i, rnd.randrange(0..0x7fff_ffff), i as i8);
  }
  for i in 0..M {
    assert_eq!(t.get(&mut root, i).unwrap().value, i as i8);
  }
}

#[test]
fn treap_persistent_implicit_key_tests() {
  const M: usize = 100;
  let mut rnd = KnuthRandom::new(12345);
  let t = treap_persistent_implicit_key::PersistentImplicitKeyTreap::new(pi_nop);
  let mut root = None;
  for i in 0..M {
    root = t.insert(root, i, rnd.randrange(0..0x7fff_ffff), i as i8);
  }
  for i in 0..M {
    assert_eq!(t.get(&root, i).unwrap().value, i as i8);
  }
}

#[test]
fn treap_tests() {
  let t = treap::Treap { relax_op: nop };
  let mut root = None;
  let mut rnd = KnuthRandom::new(12345);
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
