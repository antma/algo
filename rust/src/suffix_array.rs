pub struct SuffixArray {
  s: Vec<u8>,
  o: Vec<u32>,
  n: usize,
}

fn counting_sort(m: usize, c: &Vec<u32>, p: &Vec<u32>, o: &mut Vec<u32>) {
  let mut cnt = vec![0; m];
  for v in p {
    cnt[c[*v as usize] as usize] += 1;
  }
  for i in 1..m {
    cnt[i] += cnt[i - 1];
  }
  for pi in p.iter().rev() {
    let u = c[*pi as usize] as usize;
    cnt[u] -= 1;
    o[cnt[u]] = *pi;
  }
}

fn lcp_probe(lcp: &Vec<u32>, n: usize, l: isize, r: isize) -> u32 {
  if r - l == 1 {
    lcp[r as usize]
  } else {
    let m = (l + r) >> 1;
    lcp[n + 1 + m as usize]
  }
}

fn lcp_build(lcp: &mut Vec<u32>, n: usize, l: isize, r: isize) -> u32 {
  if r - l == 1 {
    lcp[r as usize]
  } else {
    let m = (l + r) >> 1;
    let v = lcp_build(lcp, n, l, m).min(lcp_build(lcp, n, m, r));
    lcp[n + 1 + m as usize] = v;
    v
  }
}

impl SuffixArray {
  pub fn new(s: &String) -> Self {
    let mut s = s.as_bytes().to_vec();
    s.push(0);
    let n = s.len();
    let mut c = s.iter().map(|q| *q as u32).collect::<Vec<_>>();
    let a = (1 + *c.iter().max().unwrap()) as usize;
    let mut p = vec![0u32; n];
    counting_sort(a, &c, &(0..n as u32).collect::<Vec<_>>(), &mut p);
    c[p[0] as usize] = 0;
    let mut m = 0;
    for i in 1..n {
      let pi = p[i] as usize;
      if s[pi] != s[p[i - 1] as usize] {
        m += 1;
      }
      c[pi] = m;
    }
    m += 1;
    let mut t = vec![0; n];
    let mut step = 1;
    while step < n {
      let s = step as u32;
      for q in &mut p {
        if *q < s {
          *q += n as u32;
        }
        *q -= s;
      }
      counting_sort(m as usize, &c, &p, &mut t);
      std::mem::swap(&mut t, &mut p);
      t[p[0] as usize] = 0;
      m = 0;
      let mut pj = p[0] as usize;
      for i in 1..n {
        let pi = p[i] as usize;
        if c[pi] != c[pj] || c[(pi + step) % n] != c[(pj + step) % n] {
          m += 1;
        }
        t[pi] = m;
        pj = pi;
      }
      m += 1;
      std::mem::swap(&mut c, &mut t);
      step <<= 1;
    }
    debug_assert_eq!(p[0] as usize, n - 1);
    Self {
      s,
      o: p[1..].to_vec(),
      n: n - 1,
    }
  }
  pub fn compute_lcp(&self) -> Vec<u32> {
    let n = self.n;
    let mut lcp = vec![0; 2 * n + 1];
    let mut r = vec![0; n];
    for i in 0..n {
      r[self.o[i] as usize] = i as u32;
    }
    let mut l = 0;
    for j in 0..n {
      if l > 0 {
        l -= 1;
      }
      let i = r[j] as usize;
      if i > 0 {
        let k = self.o[i - 1] as usize;
        while j + l < n && k + l < n && self.s[j + l] == self.s[k + l] {
          l += 1;
        }
      } else {
        l = 0;
      }
      lcp[i] = l as u32;
    }
    debug_assert_eq!(lcp[n], 0);
    lcp_build(&mut lcp, n, -1, n as isize);
    lcp
  }
}
