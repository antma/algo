pub struct PermutationsHeap {
  c: Vec<u8>,
  i: usize,
}

impl PermutationsHeap {
  pub fn new<T>(a: &[T]) -> Self {
    Self {
      c: vec![0; a.len()],
      i: 1,
    }
  }
  pub fn next_permutation<T>(&mut self, a: &mut [T]) -> bool {
    while self.i < a.len() {
      let i = self.i;
      let ci = self.c[i] as usize;
      if ci < i {
        a.swap(if (i & 1) == 0 { 0 } else { ci }, i);
        self.c[i] += 1;
        self.i = 1;
        return true;
      } else {
        self.c[i] = 0;
        self.i += 1;
      }
    }
    false
  }
}
