pub fn pow_mul_copied<X, P, M>(x: X, y: P, mul: M, one: X) -> X
where
  X: Copy,
  P: From<bool> + Eq + std::ops::BitAnd<Output = P> + std::ops::ShrAssign<u8> + Copy,
  M: Fn(X, X) -> X,
{
  let zero = P::from(false);
  let mut y = y;
  if y == zero {
    return one;
  }
  let one = P::from(true);
  let mut b = x;
  while (y & one) == zero {
    b = mul(b, b);
    y >>= 1;
  }
  let mut a = b;
  y >>= 1;
  while y != zero {
    b = mul(b, b);
    if (y & one) != zero {
      a = mul(a, b);
    }
    y >>= 1;
  }
  a
}

pub fn pow<X, P>(x: X, y: P, one: X) -> X
where
  X: std::ops::Mul<Output = X> + Copy,
  P: From<bool> + Eq + std::ops::BitAnd<Output = P> + std::ops::ShrAssign<u8> + Copy,
{
  pow_mul_copied(x, y, |u, v| u * v, one)
}

//for example for matrix exponentation (expensive clone)
pub fn pow_mul_ref<X, P, M, O>(x: X, y: P, mul: M, one: O) -> X
where
  X: Clone,
  P: From<bool> + Eq + std::ops::BitAnd<Output = P> + std::ops::ShrAssign<u8> + Copy,
  M: Fn(&X, &X) -> X,
  O: FnOnce() -> X,
{
  let zero = P::from(false);
  let mut y = y;
  if y == zero {
    return one();
  }
  let one = P::from(true);
  let mut b = x;
  while (y & one) == zero {
    b = mul(&b, &b);
    y >>= 1;
  }
  let mut a = b.clone();
  y >>= 1;
  while y != zero {
    b = mul(&b, &b);
    if (y & one) != zero {
      a = mul(&a, &b);
    }
    y >>= 1;
  }
  a
}
