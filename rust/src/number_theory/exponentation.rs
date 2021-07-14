pub fn generic_pow<X, P, M, O>(x: X, y: P, mul: M, one: O) -> X 
where
  X: Clone,
  P: From<u8> + Eq + std::ops::BitAnd<Output = P> + std::ops::ShrAssign<u8> + Clone,
  M: Fn(X, X) -> X,
  O: FnOnce() -> X,
{
  let zero = P::from(0);
  let mut y = y;
  if y == zero {
    return one();
  }
  let one = P::from(1);
  let mut b = x;
  while zero == (y.clone() & one.clone()) {
    b = mul(b.clone(), b);
    y >>= 1;
  }
  let mut a = b.clone();
  y >>= 1;
  while y != zero {
    b = mul(b.clone(), b);
    if (y.clone() & one.clone()) != zero {
      a = mul(a, b.clone());
    }
    y >>= 1;
  }
  a
}

pub fn pow<X, P, F>(x: X, y: P, one: F) -> X
where
  X: std::ops::Mul<Output = X> + Clone,
  P: From<u8> + Eq + std::ops::BitAnd<Output = P> + std::ops::ShrAssign<u8> + Clone,
  F: FnOnce() -> X,
{
  generic_pow(x, y, |u, v| u * v, one)
}

