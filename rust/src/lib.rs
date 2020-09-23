pub mod assignment_problem;
pub mod bigint;
pub mod kuhn;
pub mod maxflow;
pub mod polyhash;

pub use crate::bigint::BigInt;

#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
