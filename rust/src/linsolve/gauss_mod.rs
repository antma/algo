use crate::number_theory::intm_fixed::IntM;

//Ax = b
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

pub fn gauss_matrix_inverse(mut a: Vec<Vec<IntM>>) -> Option<Vec<Vec<IntM>>> {
  let n = a.len();
  let mut b = Vec::with_capacity(n);
  let one = IntM(1);
  let zero = IntM(0);
  for i in 0..n {
    let mut x = Vec::with_capacity(n);
    for j in 0..n {
      x.push(if i == j { one.clone() } else { zero.clone() });
    }
    b.push(x);
  }
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
        for u in 0..n {
          b[x][u] = b[x][u] * d - e * b[k][u];
        }
      }
    }
  }
  let mut c = Vec::with_capacity(n);
  for i in 0..n {
    a[i][i] = a[i][i].inv();
    c.push(vec![zero.clone(); n]);
  }
  for j in 0..n {
    let mut x = vec![zero.clone(); n];
    for (i, pi) in p.iter().enumerate().rev() {
      let mut t = b[i][j];
      for (p, q) in a[i].iter().zip(x.iter()).skip(i + 1) {
        t = t - *p * *q;
      }
      t = t * a[i][i];
      x[i] = t;
      c[*pi][j] = t;
    }
  }
  Some(c)
}
