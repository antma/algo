use std::ops::{AddAssign, MulAssign, SubAssign};

#[derive(Clone)]
pub struct BigInt {
  a: Vec<u32>,
}

impl BigInt {
  pub fn zero() -> Self {
    BigInt { a: vec![0u32] }
  }
  pub fn one() -> Self {
    BigInt { a: vec![1u32] }
  }
  fn add_from(&mut self, x: u32, k: usize) {
    let mut carry = x;
    for i in k..self.a.len() {
      carry += self.a[i];
      if carry >= 1_000_000_000 {
        self.a[i] = carry - 1_000_000_000;
        carry = 1;
      } else {
        self.a[i] = carry;
        carry = 0;
        break;
      }
    }
    if carry > 0 {
      self.a.push(carry);
    }
  }
}

impl std::string::ToString for BigInt {
  fn to_string(&self) -> String {
    let mut s = self.a.last().unwrap().to_string();
    for i in (0..self.a.len() - 1).rev() {
      s.push_str(&format!("{:09}", self.a[i])[..]);
    }
    s
  }
}

impl AddAssign<u32> for BigInt {
  fn add_assign(&mut self, rhs: u32) {
    assert!(rhs < 1_000_000_000);
    self.add_from(rhs, 0);
  }
}

impl AddAssign<&BigInt> for BigInt {
  fn add_assign(&mut self, rhs: &BigInt) {
    let bl = rhs.a.len();
    if self.a.len() < bl {
      self.a.resize(bl, 0);
    }
    let mut carry = 0u32;
    for i in 0..bl {
      carry += self.a[i] + rhs.a[i];
      if carry >= 1_000_000_000 {
        self.a[i] = carry - 1_000_000_000;
        carry = 1;
      } else {
        self.a[i] = carry;
        carry = 0;
      }
    }
    if carry > 0 {
      self.add_from(carry, bl);
    }
  }
}

impl SubAssign<&BigInt> for BigInt {
  fn sub_assign(&mut self, rhs: &BigInt) {
    let bl = rhs.a.len();
    let mut carry = 0i32;
    for i in 0..self.a.len() {
      carry += self.a[i] as i32;
      if i < bl {
        carry -= rhs.a[i] as i32;
      }
      if carry < 0 {
        self.a[i] = (carry + 1_000_000_000) as u32;
        carry = -1;
      } else {
        self.a[i] = carry as u32;
        carry = 0;
      }
    }
    assert!(carry >= 0);
    while self.a.len() > 1 && self.a[self.a.len() - 1] == 0 {
      self.a.pop();
    }
  }
}

impl MulAssign<u32> for BigInt {
  fn mul_assign(&mut self, rhs: u32) {
    assert!(rhs < 1_000_000_000);
    let mut carry = 0u32;
    let x64 = rhs as u64;
    for i in 0..self.a.len() {
      let c: u64 = (carry as u64) + (x64 * (self.a[i] as u64));
      self.a[i] = (c % 1_000_000_000u64) as u32;
      carry = (c / 1_000_000_000u64) as u32;
    }
    if carry > 0 {
      self.a.push(carry);
    }
  }
}
