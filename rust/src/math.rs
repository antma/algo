pub fn square_equation(a: f64, b: f64, c: f64, eps: f64) -> Vec<f64> {
  let mut t: Vec<f64> = Vec::new();
  if a.abs() < eps {
    if b.abs() > eps {
      t.push(-c / b);
    }
  } else {
    let b = b * 0.5;
    let d = b * b - a * c;
    if d > -eps {
      if d < eps {
        t.push(-b / a);
      } else {
        let d = d.sqrt();
        let t1 = (-b + d) / a;
        let t2 = (-b - d) / a;
        if t1 < t2 {
          t.push(t1);
          t.push(t2);
        } else {
          t.push(t2);
          t.push(t1);
        }
      }
    }
  }
  t
}
