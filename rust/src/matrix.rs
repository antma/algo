#[derive(Clone)]
pub struct Matrix<E> {
  rows: usize,
  columns: usize,
  a: Vec<Vec<E>>,
}

impl<E: Clone> Matrix<E> {
  pub fn identity(n: usize, zero: E, one: E) -> Self {
    let mut a = vec![vec![zero.clone(); n]; n];
    for i in 0..n {
      a[i][i] = one.clone();
    }
    Self {
      rows: n,
      columns: n,
      a,
    }
  }
}

impl<E> std::ops::Mul for Matrix<E>
where
  E: Clone + std::ops::Mul<Output = E> + std::ops::Add<Output = E>,
{
  type Output = Self;
  fn mul(self, other: Self) -> Self {
    debug_assert_eq!(self.columns, other.rows);
    let mut a = Vec::with_capacity(self.rows);
    for i in 0..self.rows {
      let mut b = Vec::with_capacity(other.columns);
      for j in 0..other.columns {
        let mut w = self.a[i][0].clone() * other.a[0][j].clone();
        for k in 1..self.columns {
          w = w + self.a[i][k].clone() * other.a[k][j].clone();
        }
        b.push(w);
      }
      a.push(b);
    }
    Matrix {
      rows: self.rows,
      columns: other.columns,
      a,
    }
  }
}
