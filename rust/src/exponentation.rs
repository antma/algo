pub fn pow<X, P, F>(x: X, y: P, one: F) -> X
where
  X: std::ops::Mul<Output = X> + Clone,
  P: From<u8> + Eq + std::ops::BitAnd<Output = P> + std::ops::ShrAssign<u8> + Clone,
  F: FnOnce() -> X,
{
  let zero = P::from(0);
  let mut y = y;
  if y == zero {
    return one();
  }
  let one = P::from(1);
  let mut b = x;
  while zero == (y.clone() & one.clone()) {
    b = b.clone() * b.clone();
    y >>= 1;
  }
  let mut a = b.clone();
  y >>= 1;
  while y != zero {
    b = b.clone() * b.clone();
    if (y.clone() & one.clone()) != zero {
      a = a * b.clone();
    }
    y >>= 1;
  }
  a
}
