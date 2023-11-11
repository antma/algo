use algo::string_functions;
#[test]
fn z_function() {
  let s = "abacaba";
  assert_eq!(
    string_functions::compute_z_function(s.as_bytes()),
    vec![0, 0, 1, 0, 3, 0, 1]
  );
}

#[test]
fn kmp() {
  let w = "ABCDABD";
  let s = "ABC ABCDAB ABCDABCDABDE";
  let p = string_functions::compute_prefix_function(w.as_bytes());
  assert_eq!(
    string_functions::KMPIterator::matches_iterator(w.as_bytes(), &p, s.as_bytes())
      .collect::<Vec<_>>(),
    vec![15]
  );
}
