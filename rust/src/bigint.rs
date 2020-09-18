#[derive(Clone)]
pub struct BigInt {
  a: Vec<u32>,
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

impl BigInt {
  pub fn zero() -> Self {
    BigInt { a: vec![0u32] }
  }
  pub fn one() -> Self {
    BigInt { a: vec![1u32] }
  }
  pub fn mul_int(&mut self, x: u32) {
    let mut carry = 0u32;
    let x64 = x as u64;
    for i in 0..self.a.len() {
      let c: u64 = (carry as u64) + (x64 * (self.a[i] as u64));
      self.a[i] = (c % 1_000_000_000u64) as u32;
      carry = (c / 1_000_000_000u64) as u32;
    }
    if carry > 0 {
      self.a.push(carry);
    }
  }
  pub fn incr(&mut self, b: &BigInt) {
    let bl = b.a.len();
    if self.a.len() < bl {
      self.a.resize(bl, 0);
    }
    let mut carry = 0u32;
    for i in 0..self.a.len() {
      carry += self.a[i];
      if i < bl {
        carry += b.a[i];
      }
      if carry >= 1_000_000_000 {
        self.a[i] = carry - 1_000_000_000;
        carry = 1;
      } else {
        self.a[i] = carry;
        carry = 0;
      }
    }
    if carry > 0 {
      self.a.push(carry);
    }
  }
  pub fn decr(&mut self, b: &BigInt) {
    let bl = b.a.len();
    let mut carry = 0i32;
    for i in 0..self.a.len() {
      carry += self.a[i] as i32;
      if i < bl {
        carry -= b.a[i] as i32;
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
