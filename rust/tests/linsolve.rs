use algo::linsolve::gauss_mod::{gauss_matrix_inverse, gauss_mod};
use algo::number_theory::intm_fixed::IntM;

#[test]
fn linsolve_gauss_mod_tests() {
  let a = vec![
    vec![IntM(1), IntM(2), IntM(3)],
    vec![IntM(0), IntM(0), IntM(1)],
    vec![IntM(0), IntM(1), IntM(0)],
  ];
  assert_eq!(
    gauss_mod(a.clone(), vec![IntM(1), IntM(2), IntM(3)]),
    Some(vec![IntM(1) - IntM(12), IntM(3), IntM(2)])
  );
  assert_eq!(
    gauss_matrix_inverse(a.clone()),
    Some(vec![
      vec![IntM(1), IntM(0) - IntM(3), IntM(0) - IntM(2)],
      vec![IntM(0), IntM(0), IntM(1)],
      vec![IntM(0), IntM(1), IntM(0)]
    ])
  );
}
