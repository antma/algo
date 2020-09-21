mod tests {
  #[test]
  fn hand1() {
    let mut a = algo::bigint::BigInt::one();
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
  fn fibs_test() {
    let mut a = algo::bigint::BigInt::one();
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
  }
}
