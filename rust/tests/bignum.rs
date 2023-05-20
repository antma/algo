use algo::bignum::bigint::BigInt;
use algo::bignum::ubigint::UBigInt;
use std::str::FromStr;

#[test]
fn hand1() {
  let mut a = UBigInt::one();
  a *= 123456789;
  assert_eq!(a.to_string(), "123456789");
  let mut b = a.clone();
  b *= 123456789;
  assert_eq!(b.to_string(), "15241578750190521");
  let mut c = b.clone();
  c -= &a;
  assert_eq!(c.to_string(), "15241578626733732");
  let mut d = c.clone();
  d += &a;
  assert_eq!(d.to_string(), b.to_string());
}
#[test]
fn fibs() {
  let mut a = UBigInt::one();
  let mut b = a.clone();
  for _i in 2..=99 {
    let c = b.clone();
    b += &a;
    a = c;
  }
  const FIB99: &str = "354224848179261915075";
  assert_eq!(b.to_string(), FIB99);
  for _i in 100..=999 {
    let c = b.clone();
    b += &a;
    a = c;
  }
  const FIB999: &str = "43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875";
  assert_eq!(b.to_string(), FIB999);
  for _i in 100..=999 {
    let c = a.clone();
    b -= &a;
    a = b;
    b = c;
  }
  assert_eq!(b.to_string(), FIB99);
  assert_eq!(b, UBigInt::from_str(FIB99).unwrap());
}
#[test]
fn factorial() {
  let a = compute_factorial_by_seq_multiplications(100);
  assert_eq!(a.to_string(), "93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000");
}
#[test]
fn binomial() {
  let mut a = UBigInt::one();
  for i in 51..=100 {
    a *= i;
  }
  for i in 2..=50 {
    a /= i;
  }
  //binomial(100, 50)
  assert_eq!(a.to_string(), "100891344545564193334812497256");
}
#[test]
fn comparision() {
  assert!(UBigInt::one() == UBigInt::one());
  assert!(UBigInt::one() > UBigInt::zero());
  assert!(UBigInt::zero() < UBigInt::one());
  let mut x = UBigInt::one();
  for _ in 0..20 {
    x *= 10;
    assert!(UBigInt::one() < x);
  }
}

#[test]
fn test_from_i8() {
  assert_eq!(BigInt::from(127) + BigInt::from(-127), BigInt::from(0));
  assert_eq!(BigInt::from(127) + BigInt::from(-126), BigInt::from(1));
  assert_eq!(BigInt::from(126) + BigInt::from(-127), BigInt::from(-1));
}

#[test]
fn test_display() {
  assert_eq!(
    (BigInt::from_str("-857").unwrap() * BigInt::from_str("14405693").unwrap()).to_string(),
    "-12345678901"
  );
}

#[test]
fn test_signed_bigint() {
  let one = BigInt::from_str("1").unwrap();
  let neg_two = BigInt::from_str("-2").unwrap();
  let neg_one = BigInt::from_str("-1").unwrap();
  let two = BigInt::from_str("2").unwrap();
  assert_eq!(one.clone() + neg_two.clone(), neg_one.clone());
  assert_eq!(one.clone() + one.clone(), two.clone());
  assert_eq!(neg_one.clone() + neg_one.clone(), neg_two.clone());
  assert_eq!(neg_two.clone() + one.clone(), neg_one.clone());
  assert_eq!(one.clone() - two.clone(), neg_one.clone());
  assert_eq!(two.clone() - one.clone(), one.clone());
  assert_eq!(
    neg_two.clone() * two.clone(),
    BigInt::from_str("-4").unwrap()
  );
  assert_eq!(two.clone() * two.clone(), BigInt::from_str("4").unwrap());
  assert_eq!(
    neg_two.clone() * neg_two.clone(),
    BigInt::from_str("4").unwrap()
  );
  let zero = BigInt::from_str("0").unwrap();
  assert_eq!(BigInt::from_str("-0").unwrap(), zero);
  assert_eq!(BigInt::from_str("-0").unwrap() - zero.clone(), zero.clone());
  assert_eq!(BigInt::from_str("-0").unwrap() + zero.clone(), zero.clone());
  assert_eq!(zero.clone() - BigInt::from_str("-0").unwrap(), zero.clone());
  assert_eq!(zero.clone() * BigInt::from_str("-0").unwrap(), zero.clone());
  let m = BigInt::from_str("1000000000000000000").unwrap();
  let m2 = BigInt::from_str("-1000000000000000000").unwrap();
  assert_eq!(m.clone() + m2, zero.clone());
  assert_eq!(zero.clone() * m.clone(), zero.clone());
  assert_eq!(m * zero.clone(), zero.clone());
}

fn compute_factorial_by_seq_multiplications(n: u32) -> UBigInt {
  let mut x = UBigInt::from(1);
  for i in 2..=n {
    x *= i;
  }
  x
}

fn compute_factorial_by_two_bigint_multiplication(n: u32) -> UBigInt {
  let mut u = UBigInt::from(1);
  let mut v = UBigInt::from(1);
  for i in 2..=n {
    if u < v {
      u *= i;
    } else {
      v *= i;
    }
  }
  u *= &v;
  u
}

#[test]
fn test_ubigint_mul() {
  let mut u = UBigInt::from(9);
  u *= &UBigInt::from(8);
  assert_eq!(u.to_string(), "72");
  for n in vec![200, 300, 500, 750, 1000, 2000] {
    assert_eq!(
      compute_factorial_by_seq_multiplications(n),
      compute_factorial_by_two_bigint_multiplication(n)
    );
  }
}
