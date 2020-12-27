use std::ops::Mul;

fn linear_sieve<T: From<i8> + Clone + Copy + Mul<Output = T>>(
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
