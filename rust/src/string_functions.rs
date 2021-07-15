pub fn compute_z_function<T: Eq>(s: &[T]) -> Vec<usize> {
  let n = s.len();
  let mut l = 0;
  let mut r = 0;
  let mut z = Vec::with_capacity(n);
  z.push(0);
  for i in 1..n {
    let mut k = if i <= r { (r - i + 1).min(z[i - l]) } else { 0 };
    while i + k < n && s[k] == s[i + k] {
      k += 1;
    }
    let j = i + k - 1;
    if j > r {
      l = i;
      r = j;
    }
    z.push(k);
  }
  z
}
