pub struct PrimeTable {
  n: usize,
  a: Vec<u32>,
}

impl PrimeTable {
  pub fn new(n: usize) -> Self {
    let m = n / 2;
    let mut a = vec![0xffffffffu32; (n + 63) >> 6];
    a[0] -= 1;
    let mut i = 1usize;
    while i * i < n {
      if (a[i >> 5] & (1u32 << (i & 31))) != 0 {
        for j in (2 * i * (i + 1)..m).step_by(2 * i + 1) {
          a[j >> 5] &= !(1u32 << (j & 31));
        }
      }
      i += 1;
    }
    Self { n, a }
  }
  pub fn is_prime(&self, n: u32) -> bool {
    match n.cmp(&2) {
      std::cmp::Ordering::Less => false,
      std::cmp::Ordering::Equal => true,
      std::cmp::Ordering::Greater => {
        (n & 1) != 0 && (self.a[(n >> 6) as usize] & (1u32 << ((n >> 1) & 31))) != 0
      }
    }
  }
  pub fn primes(&self) -> Vec<u32> {
    let mut b = Vec::new();
    if self.n <= 2 {
      return b;
    }
    b.push(2);
    for i in (3..self.n).step_by(2) {
      if (self.a[i >> 6] & (1u32 << ((i >> 1) & 31))) != 0 {
        b.push(i as u32);
      }
    }
    b
  }
}

pub fn factorization(mut n: u32, primes: &Vec<u32>) -> Vec<(u32, u8)> {
  let mut f = Vec::new();
  for i in primes
    .iter()
    .cloned()
    .chain((primes.last().unwrap() + 2..).step_by(2))
  {
    if i * i > n {
      break;
    }
    if n % i == 0 {
      let mut c = 1;
      n /= i;
      while n % i == 0 {
        n /= i;
        c += 1;
      }
      f.push((i, c));
    }
  }
  if n > 1 {
    f.push((n, 1));
  }
  f
}
