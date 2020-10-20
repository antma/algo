pub mod assignment_problem;
pub mod bigint;
pub mod disjoint_set;
pub mod geometry;
pub mod integration;
pub mod intm;
pub mod kuhn;
pub mod math;
pub mod maxflow;
pub mod polyhash;
pub mod random;
pub mod segment_tree;
pub mod sieve;
pub mod splay_tree;

pub use crate::assignment_problem::AssignmentProblem;
pub use crate::bigint::BigInt;
pub use crate::disjoint_set::DisjointSetWithSize;

#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
