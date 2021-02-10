use crate::bigint::ubigint::UBigInt;
use std::ops::{Add, Mul, Sub};

#[derive(Clone, Debug)]
pub struct BigInt {
  negative: bool,
  x: UBigInt,
}

impl From<i8> for BigInt {
  fn from(i: i8) -> Self {
    if i >= 0 {
      Self {
        negative: false,
        x: UBigInt::from(i),
      }
    } else {
      Self {
        negative: true,
        x: UBigInt::from(i.abs()),
      }
    }
  }
}

impl Ord for BigInt {
  fn cmp(&self, other: &BigInt) -> std::cmp::Ordering {
    if self.negative && !other.negative {
      return std::cmp::Ordering::Less;
    }
    if !self.negative && other.negative {
      return std::cmp::Ordering::Greater;
    }
    if !self.negative {
      self.x.cmp(&other.x)
    } else {
      other.x.cmp(&self.x)
    }
  }
}

impl PartialOrd for BigInt {
  fn partial_cmp(&self, other: &BigInt) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl PartialEq for BigInt {
  fn eq(&self, other: &BigInt) -> bool {
    self.negative == other.negative && self.x == other.x
  }
}
impl Eq for BigInt {}

impl Add for BigInt {
  type Output = Self;
  fn add(self, other: BigInt) -> Self {
    if self.negative == other.negative {
      let mut w = self.x;
      w += &other.x;
      Self {
        negative: self.negative,
        x: w,
      }
    } else {
      let (u, v) = {
        if !self.negative {
          (self, other)
        } else {
          (other, self)
        }
      };
      if u.x >= v.x {
        let mut w = u.x;
        w -= &v.x;
        Self {
          negative: false,
          x: w,
        }
      } else {
        let mut w = v.x;
        w -= &u.x;
        Self {
          negative: true,
          x: w,
        }
      }
    }
  }
}

impl Sub for BigInt {
  type Output = Self;
  fn sub(self, other: BigInt) -> Self {
    let v = BigInt {
      negative: !other.negative,
      x: other.x,
    };
    self + v
  }
}

impl Mul for BigInt {
  type Output = Self;
  fn mul(self, other: BigInt) -> Self {
    Self {
      negative: self.negative != other.negative,
      x: {
        let mut w = self.x;
        w *= &other.x;
        w
      },
    }
  }
}

impl std::str::FromStr for BigInt {
  type Err = String;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    if s.is_empty() {
      return Err("empty string".to_string());
    }
    let t = if s.starts_with('-') {
      let (_, u) = s.split_at(1);
      BigInt {
        negative: true,
        x: UBigInt::from_str(u)?,
      }
    } else {
      BigInt {
        negative: false,
        x: UBigInt::from_str(s)?,
      }
    };
    Ok(t)
  }
}
