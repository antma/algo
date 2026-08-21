use crate::number_theory::intm::{addm, mulm, subm, powm};
use crate::fft::Revbin;

const MODULO: u32 = 998_244_353;//(2^23)*7*17+1
/*  PARI/GP
? znprimroot(998244353)
%1 = Mod(3, 998244353)
*/
const G: u32 = 3;//primitive root

fn ntt_transform(revbin: &mut Revbin, a: &mut [u32], inversion: bool) {
  let n = revbin.n;
  revbin.permute(a);
  let mut len = 2usize;
  while len <= n {
    debug_assert_eq!((MODULO - 1) % (len as u32), 0);
    let mut wlen = powm(G, (MODULO - 1) / (len as u32), MODULO); 
    if inversion {
      wlen = powm(wlen, MODULO - 2, MODULO);
    }
    for i in (0 .. n).step_by(len) {
      let mut w = 1;
      for j in 0 .. len / 2 {
        let u = a[i + j];
        let v = mulm(a[i + j + len / 2], w, MODULO);
        a[i+j] = addm(u, v, MODULO);
        a[i + j + len / 2] = subm(u, v, MODULO);
        w = mulm(w, wlen, MODULO);
      }
    }
    len *= 2;
  }
  if inversion {
    let inv_n = powm(n as u32, MODULO - 2, MODULO);
    for w in a.iter_mut() {
      *w = mulm(*w, inv_n, MODULO);
    }
  }
}

//a := a * b
pub fn ntt_multiply(revbin: &mut Revbin, a: &mut [u32], b: &mut [u32]) {
  debug_assert_eq!(a.len(), b.len());
  ntt_transform(revbin, a, false);
  ntt_transform(revbin, b, false);
  for (u, v) in a.iter_mut().zip(b.iter()) {
    *u = mulm(*u, *v, MODULO);
  }
  ntt_transform(revbin, a, true);
}
