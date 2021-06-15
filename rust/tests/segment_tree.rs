use algo::segment_tree::SegmentTree;

fn f(x: &i8, y: &i8) -> i8 {
  *x.min(y)
}

#[test]
fn min_test() {
  let m = 100usize;
  let mut x = 0i8;
  let mut y = 1i8;
  let mut a: Vec<i8> = Vec::new();
  for _ in 0..m {
    let t = x.wrapping_add(y);
    x = y;
    y = t;
    a.push(y);
  }
  let st = SegmentTree::new(a.clone(), f);
  for i in 0..m {
    let mut acc = a[i];
    assert_eq!(st.reduce(0x7fi8, i, i + 1), a[i]);
    for j in i + 1..m {
      acc = acc.min(a[j]);
      assert_eq!(st.reduce(0x7fi8, i, j + 1), acc);
    }
  }
}
