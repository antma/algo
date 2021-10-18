use crate::bitset::BitSet;
use crate::number_theory::primes::PrimeTable;

pub struct SegmentedSieve {
  idx: Vec<u16>,
  tbl: Vec<u16>,
  delta: Vec<u8>,
}

impl SegmentedSieve {
  const P: u16 = 30_030;
  const P64: u64 = Self::P as u64;
  const L: usize = 5760;
  const P32: u32 = Self::P as u32;
  const SMALL_PRIMES: [u32; 6] = [2, 3, 5, 7, 11, 13];
  pub fn new() -> Self {
    let mut b = vec![0u16; Self::P as usize];
    for i in Self::SMALL_PRIMES.iter() {
      for j in (0..Self::P).step_by(*i as usize) {
        b[j as usize] = 0xffff;
      }
    }
    let idx = (0..Self::P as u16)
      .filter(|i| b[*i as usize] == 0)
      .collect::<Vec<_>>();
    debug_assert_eq!(Self::L, idx.len());
    let mut d = vec![0; Self::L];
    for i in 0..Self::L - 1 {
      d[i] = (idx[i + 1] - idx[i]) as u8;
    }
    d[Self::L - 1] = (idx[0] + Self::P - idx[Self::L - 1]) as u8;
    for (i, k) in idx.iter().enumerate() {
      b[*k as usize] = i as u16;
    }
    for i in (0..b.len()).rev() {
      if b[i] == 0xffff {
        b[i] = b[i + 1];
      }
    }
    Self {
      idx,
      delta: d,
      tbl: b,
    }
  }
  pub fn process<T, F: Fn(&mut T, u64) -> ()>(
    &self,
    range: std::ops::Range<u64>,
    block_size: usize,
    state: &mut T,
    f: F,
  ) {
    let u = range.start;
    let v = range.end;
    let blocks = (block_size as u64 + (Self::P64 - 1)) / Self::P64;
    let p = PrimeTable::new(((v as f64).sqrt() as usize).max(Self::P as usize) + 100).primes();
    let mut i = u / Self::P64;
    if i == 0 {
      for q in &p {
        let q64 = *q as u64;
        if q64 >= v {
          return;
        }
        if *q >= Self::P as u32 {
          break;
        }
        if q64 >= u {
          f(state, q64);
        }
      }
      i += 1;
    }
    let j = (v + (Self::P64 - 1)) / Self::P64;
    let mut bs = BitSet::new(blocks as usize * Self::L);
    while i < j {
      let off = i * Self::P64;
      let step = (j - i).min(blocks);
      let e = ((i + step) * Self::P64).min(v);
      let m = (e - off) as u32;
      bs.set_all();
      for q in &p[6..] {
        let q32 = *q;
        let q64 = q32 as u64;
        if q64 * q64 >= e {
          break;
        }
        let mut u = (off + (q64 - 1)) / q64;
        let v = u % Self::P64;
        let mut k = self.tbl[v as usize] as usize;
        u -= v;
        u += self.idx[k] as u64;
        let mut o = (u * q64 - off) as u32;
        while o < m {
          let y = o / Self::P32;
          let x = self.tbl[(o % Self::P32) as usize] as usize + y as usize * Self::L;
          bs.clear(x);
          o += q32 * self.delta[k] as u32;
          k += 1;
          if k == Self::L {
            k = 0;
          }
        }
      }
      for j in bs.bits() {
        let x = off + self.idx[j % Self::L] as u64 + (j / Self::L) as u64 * Self::P64;
        if x >= v {
          return;
        }
        if x >= u {
          f(state, x);
        }
      }
      i += step;
    }
  }
}
