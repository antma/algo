const MODULO: u32 = 1_000_000_007;

#[derive(Clone, Copy)]
pub struct IntM(pub u32);

impl std::ops::Add for IntM {
  type Output = Self;
  fn add(self, other: Self) -> Self {
    let r = self.0.wrapping_add(other.0);
    IntM(if r < self.0 || r >= MODULO {
      r.wrapping_sub(MODULO)
    } else {
      r
    })
  }
}

impl std::ops::Sub for IntM {
  type Output = Self;
  fn sub(self, other: Self) -> Self {
    let r = self.0.wrapping_sub(other.0);
    IntM(if self.0 < other.0 {
      r.wrapping_add(MODULO)
    } else {
      r
    })
  }
}

impl std::ops::Mul for IntM {
  type Output = Self;
  fn mul(self, other: Self) -> Self {
    IntM((((self.0 as u64) * (other.0 as u64)) % (MODULO as u64)) as u32)
  }
}

fn gcdext(a: i32, b: i32) -> (i32, i32, i32) {
  if b == 0 {
    (a, 1, 0)
  } else {
    let (res, y, x) = gcdext(b, a % b);
    (res, x, y - x * (a / b))
  }
}

impl IntM {
  pub fn inv(self) -> IntM {
    let (g, _, x) = gcdext(MODULO as i32, self.0 as i32);
    assert_eq!(g, 1);
    IntM((if x < 0 { x + MODULO as i32 } else { x }) as u32)
  }
}
