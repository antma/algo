mod tests {
  #[test]
  fn hand1() {
    let a: Vec<Vec<i32>> = vec![
      vec![7, 53, 183, 439, 863],
      vec![497, 383, 563, 79, 973],
      vec![287, 63, 343, 169, 583],
      vec![627, 343, 773, 959, 943],
      vec![767, 473, 103, 699, 303],
    ];
    let mut g = algo::assignment_problem::AssignmentProblem::new(a);
    assert_eq!(g.maximize(i32::MAX), 3315);
  }
}
