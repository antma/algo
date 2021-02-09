pub mod assignment_problem;
pub mod bigint;
pub mod bits;
pub mod disjoint_set;
pub mod fenwick_tree;
pub mod fht;
pub mod geometry;
pub mod integration;
pub mod intm;
pub mod kuhn;
pub mod linear_sieve;
pub mod math;
pub mod maxflow;
pub mod polyhash;
pub mod primes;
pub mod random;
pub mod ratio;
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
