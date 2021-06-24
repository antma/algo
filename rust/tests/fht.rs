use algo::fht::FHT;

fn slow_conv(x: &Vec<f64>, y: &Vec<f64>) -> Vec<f64> {
  let n = x.len();
  let mut z = vec![0.0; n];
  for t in 0..n {
    for i in 0..n {
      z[t] += x[i] * y[(t + n - i) % n];
    }
  }
  z
}

fn check(mut x: Vec<f64>, mut y: Vec<f64>) {
  let n = x.len();
  let f = slow_conv(&x, &y);
  let mut fht = FHT::new(n);
  fht.conv(&mut x, &mut y);
  for i in 0..n {
    assert!((f[i] - y[i]).abs() < 0.5, "{:?} and {:?}", f, y);
  }
}

#[test]
fn slow() {
  check(
    vec![0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0],
    vec![0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0],
  );
}
