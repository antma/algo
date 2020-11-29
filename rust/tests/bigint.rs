use algo::bigint::BigInt;
use std::str::FromStr;

#[test]
fn hand1() {
  let mut a = BigInt::one();
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
  let mut a = BigInt::one();
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
  assert_eq!(b, BigInt::from_str(FIB99).unwrap());
}
#[test]
fn factorial() {
  let mut a = BigInt::one();
  for i in 2..=100 {
    a *= i;
  }
  assert_eq!(a.to_string(), "93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000");
}
#[test]
fn binomial() {
  let mut a = BigInt::one();
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
  assert!(BigInt::one() == BigInt::one());
  assert!(BigInt::one() > BigInt::zero());
  assert!(BigInt::zero() < BigInt::one());
  let mut x = BigInt::one();
  for _ in 0..20 {
    x *= 10;
    assert!(BigInt::one() < x);
  }
}
