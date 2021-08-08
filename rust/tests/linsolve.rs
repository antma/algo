use algo::linsolve::gauss_mod::gauss_mod;
use algo::number_theory::intm_fixed::IntM;

#[test]
fn linsolve_gauss_mod_tests() {
  assert_eq!(
    gauss_mod(
      vec![
        vec![IntM(1), IntM(2), IntM(3)],
        vec![IntM(0), IntM(0), IntM(1)],
        vec![IntM(0), IntM(1), IntM(0)]
      ],
      vec![IntM(1), IntM(2), IntM(3)]
    ),
    Some(vec![IntM(1) - IntM(12), IntM(3), IntM(2)])
  );
}
