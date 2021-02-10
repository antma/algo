use std::cmp::Ordering;
use std::ops::{AddAssign, DivAssign, MulAssign, SubAssign};

#[derive(Clone, Debug)]
pub struct UBigInt {
  a: Vec<u32>,
}

impl From<i8> for UBigInt {
  fn from(i: i8) -> Self {
    Self { a: vec![i as u32] }
  }
}

impl UBigInt {
  pub fn zero() -> Self {
    UBigInt { a: vec![0u32] }
  }
  pub fn one() -> Self {
    UBigInt { a: vec![1u32] }
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
  fn remove_leading_zeros(&mut self) {
    let n = self.a.len();
    let mut k = n - 1;
    while k > 0 && self.a[k] == 0 {
      k -= 1;
    }
    k += 1;
    if k < n {
      self.a.truncate(k);
    }
  }
}

impl std::fmt::Display for UBigInt {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.a.last().unwrap())?;
    for p in self.a.iter().rev().skip(1) {
      write!(f, "{:09}", *p)?;
    }
    Ok(())
  }
}

impl AddAssign<u32> for UBigInt {
  fn add_assign(&mut self, rhs: u32) {
    assert!(rhs < 1_000_000_000);
    self.add_from(rhs, 0);
  }
}

impl AddAssign<&UBigInt> for UBigInt {
  fn add_assign(&mut self, rhs: &UBigInt) {
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

impl SubAssign<&UBigInt> for UBigInt {
  fn sub_assign(&mut self, rhs: &UBigInt) {
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
    self.remove_leading_zeros();
  }
}

impl MulAssign<u32> for UBigInt {
  fn mul_assign(&mut self, rhs: u32) {
    assert!(rhs < 1_000_000_000);
    let mut carry = 0u64;
    let x64 = rhs as u64;
    for x in self.a.iter_mut() {
      carry += x64 * (*x as u64);
      *x = (carry % 1_000_000_000) as u32;
      carry /= 1_000_000_000;
    }
    if carry > 0 {
      self.a.push(carry as u32);
    }
  }
}

impl MulAssign<&UBigInt> for UBigInt {
  fn mul_assign(&mut self, rhs: &UBigInt) {
    let mut r = UBigInt::zero();
    for (i, j) in rhs.a.iter().enumerate() {
      if *j == 0 {
        continue;
      }
      let mut v = vec![0u32; i];
      let mut x = self.clone();
      v.append(&mut x.a);
      x.a = v;
      x *= *j;
      r += &x;
    }
    self.a = r.a;
  }
}

impl DivAssign<u32> for UBigInt {
  fn div_assign(&mut self, rhs: u32) {
    assert!(rhs < 1_000_000_000);
    let mut d: u64 = 0;
    for x in self.a.iter_mut().rev() {
      d *= 1_000_000_000;
      d += *x as u64;
      *x = (d / (rhs as u64)) as u32;
      d -= (*x as u64) * (rhs as u64);
    }
    self.remove_leading_zeros();
  }
}

impl Ord for UBigInt {
  fn cmp(&self, other: &UBigInt) -> Ordering {
    if self.a.len() > other.a.len() {
      return Ordering::Greater;
    }
    if self.a.len() < other.a.len() {
      return Ordering::Less;
    }
    for i in (0..self.a.len()).rev() {
      if self.a[i] > other.a[i] {
        return Ordering::Greater;
      }
      if self.a[i] < other.a[i] {
        return Ordering::Less;
      }
    }
    Ordering::Equal
  }
}

impl PartialOrd for UBigInt {
  fn partial_cmp(&self, other: &UBigInt) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl PartialEq for UBigInt {
  fn eq(&self, other: &UBigInt) -> bool {
    self.a == other.a
  }
}
impl Eq for UBigInt {}

impl std::str::FromStr for UBigInt {
  type Err = String;
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let c = s.as_bytes();
    for (i, k) in c.iter().enumerate() {
      if *k < 48 || *k > 57 {
        return Err(format!(
          "illegal character {} at {} position",
          *k as char, i
        ));
      }
    }
    let l = c.len();
    let mut a = Vec::with_capacity((l + 8) / 9);
    let mut i = l;
    while i > 0 {
      let j = if i >= 9 { i - 9 } else { 0 };
      a.push(u32::from_str(&s[j..i]).unwrap());
      i = j;
    }
    Ok(UBigInt { a })
  }
}
