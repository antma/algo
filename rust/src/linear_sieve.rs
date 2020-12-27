fn linear_sieve<T: From<i8> + Clone + Copy + std::ops::Mul<Output = T>>(
  n: usize,
  prime: fn(i32) -> T,
  divides: fn(T, i32) -> T,
) -> Vec<T> {
  let mut composite = vec![0u32; (n + 31) >> 5];
  let mut f = vec![T::from(0); n];
  f[1] = T::from(1);
  let mut primes = Vec::new();
  for i in 2..n as i32 {
    if (composite[(i >> 5) as usize] & (1 << (i & 31))) == 0 {
      primes.push(i);
      f[i as usize] = prime(i);
    }
    for &j in &primes {
      let k = i * j;
      if k >= n as i32 {
        break;
      }
      composite[(k >> 5) as usize] |= 1 << (k & 31);
      if (i % j) == 0 {
        f[k as usize] = divides(f[i as usize], j);
        break;
      } else {
        f[k as usize] = f[i as usize] * f[j as usize];
      }
    }
  }
  f
}

pub fn mus(n: usize) -> Vec<i8> {
  linear_sieve(n, |_| -1, |_, _| 0)
}

pub fn totients(n: usize) -> Vec<i32> {
  linear_sieve(n, |p| p - 1, |acc, j| acc * j)
}

//sum_{1<=k<=n} = f(floor(n / k))
pub struct HarmonicSeries<T> {
  n: T,
  cur: T,
  front: T,
}

impl<T: From<u8> + Copy> HarmonicSeries<T> {
  pub fn new(n: T) -> Self {
    Self {
      n,
      cur: n,
      front: T::from(1),
    }
  }
}

impl<
    T: From<u8> + PartialOrd + std::ops::Add<Output = T> + std::ops::Div<Output = T> + Ord + Copy,
  > std::iter::Iterator for HarmonicSeries<T>
{
  type Item = (std::ops::Range<T>, T);
  fn next(&mut self) -> Option<Self::Item> {
    if self.front > self.n {
      None
    } else {
      let f = self.front;
      let c = self.cur;
      self.front = T::from(1) + (self.n / c);
      if f > self.n {
        self.front = self.n + T::from(1);
      } else {
        self.cur = self.n / self.front;
      }
      Some((f..self.front, c))
    }
  }
}
