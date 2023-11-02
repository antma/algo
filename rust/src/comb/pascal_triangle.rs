pub struct PascalTriangle<T>(Vec<Vec<T>>);

impl<T> PascalTriangle<T>
  where T: Default + Clone + From<u8> + std::ops::Add<T, Output=T>
{
  pub fn new(n: usize) -> Self {
    let mut a: Vec<Vec<T>> = Vec::new();
    a.push(vec![T::from(1u8)]);
    for i in 1 ..= n {
      let mut b: Vec<T> = Vec::with_capacity(i + 1);
      b.push(T::from(1u8));
      for (p1, p2) in a[i-1].iter().zip(a[i-1].iter().skip(1)) {
        b.push(p1.clone() + p2.clone());
      }
      b.push(T::from(1u8));
      a.push(b);
    }
    PascalTriangle(a)
  }
  pub fn binomial(&self, n: usize, k: usize) -> T {
    if k > n { T::from(0u8) }
    else { self.0[n][k].clone() }
  }
}
