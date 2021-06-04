#[test]
fn z_function() {
  let s = "abacaba";
  assert_eq!(
    algo::string_functions::compute_z_function(s.as_bytes()),
    vec![0, 0, 1, 0, 3, 0, 1]
  );
}
