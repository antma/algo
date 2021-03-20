#[derive(Clone)]
pub struct Complex<T> {
  pub re: T,
  pub im: T,
}

impl<T> Complex<T> {
  pub fn new(re: T, im: T) -> Self {
    Complex { re, im }
  }
}

impl<T> std::ops::Mul for Complex<T>
where
  T: Clone + std::ops::Add<Output = T> + std::ops::Sub<Output = T> + std::ops::Mul<Output = T>,
{
  type Output = Self;
  fn mul(self, rhs: Self) -> Self {
    Complex::new(
      self.re.clone() * rhs.re.clone() - self.im.clone() * rhs.im.clone(),
      self.re.clone() * rhs.im.clone() + self.im.clone() * rhs.re.clone(),
    )
  }
}

impl<T> PartialEq for Complex<T>
where
  T: PartialEq,
{
  fn eq(&self, other: &Self) -> bool {
    self.re == other.re && self.im == other.im
  }
}
