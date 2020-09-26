use algo::integration::simpson;

#[test]
fn hand() {
  const EPS: f64 = 1e-6;
  assert!((simpson(0.0, 1.0, 1e-6, |x| x) - 0.5).abs() < EPS);
  assert!((simpson(0.0, 1.0, 1e-6, |x| x * x) - 1.0 / 3.0).abs() < EPS);
  assert!(simpson(-1.0, 1.0, 1e-6, |x| x.sin()).abs() < EPS);
  assert!((simpson(-1.0, 1.0, 1e-6, |x| x.cos()) - 2.0 * 1.0f64.sin()).abs() < 10.0 * EPS);
}
