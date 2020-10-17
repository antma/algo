pub fn mulm(x: u32, y: u32, m: u32) -> u32 {
  (((x as u64) * (y as u64)) % (m as u64)) as u32
}

pub fn powm(x: u32, y: u32, m: u32) -> u32 {
  if y == 0 {
    return 1;
  }
  let mut y = y;
  let mut b = x;
  while 0 == (y & 1) {
    b = mulm(b, b, m);
    y >>= 1;
  }
  let mut a = b;
  y >>= 1;
  while y != 0 {
    b = mulm(b, b, m);
    if (y & 1) != 0 {
      a = mulm(a, b, m);
    }
    y >>= 1;
  }
  a
}
