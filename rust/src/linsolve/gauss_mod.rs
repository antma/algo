use crate::number_theory::intm_fixed::IntM;

pub fn gauss_mod(mut a: Vec<Vec<IntM>>, mut b: Vec<IntM>) -> Option<Vec<IntM>> {
  let n = a.len();
  let mut p = (0..n).collect::<Vec<_>>();
  for k in 0..n {
    let mut q = None;
    for (x, p) in a.iter().enumerate().skip(k) {
      if let Some(y) = p.iter().skip(k).position(|q| q.0 != 0) {
        q = Some((x, y + k));
        break;
      }
    }
    q?;
    let (i, j) = q.unwrap();
    if k < i {
      a.swap(k, i);
      b.swap(k, i);
    }
    if k < j {
      for q in &mut a {
        q.swap(k, j);
      }
      p.swap(k, j);
    }
    let d = a[k][k];
    for x in k + 1..n {
      let e = a[x][k];
      if e.0 != 0 {
        for y in k + 1..n {
          a[x][y] = a[x][y] * d - e * a[k][y];
        }
        b[x] = b[x] * d - e * b[k];
      }
    }
  }
  let mut x = vec![IntM(0); n];
  for i in (0..n).rev() {
    let mut t = b[i];
    for k in i + 1..n {
      t = t - a[i][k] * x[p[k]];
    }
    x[p[i]] = t * a[i][i].inv();
  }
  Some(x)
}
