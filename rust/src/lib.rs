pub mod assignment_problem;
pub mod bigint;
pub mod bits;
pub mod bitset;
pub mod complex;
pub mod disjoint_set;
pub mod fenwick_tree;
pub mod fht;
pub mod geometry;
pub mod integration;
pub mod kuhn;
pub mod lazy_propagation;
pub mod linear_sieve;
pub mod linsolve;
pub mod math;
pub mod matrix;
pub mod maxflow;
pub mod mo;
pub mod number_theory;
pub mod polyhash;
pub mod random;
pub mod ratio;
pub mod segment_tree;
pub mod splay_tree;
pub mod string_functions;
pub mod tree;

pub use crate::assignment_problem::AssignmentProblem;
pub use crate::bigint::ubigint::UBigInt;
pub use crate::disjoint_set::DisjointSetWithSize;

#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
