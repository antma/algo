pub struct Node<K, V, E> {
  pub key: K,
  pub value: V,
  pub extra: E,
  left: Option<Box<Node<K, V, E>>>,
  right: Option<Box<Node<K, V, E>>>,
  y: i32,
  sz: u32,
}

pub struct Treap<K, V, E> {
  pub relax_op: fn(&mut Node<K, V, E>),
}

impl<K: PartialOrd + Clone, V, E: Default> Treap<K, V, E> {
  #[inline]
  fn relax_inc(&self, t: &mut Node<K, V, E>) {
    (self.relax_op)(t);
    t.sz += 1;
  }
  #[inline]
  fn relax_dec(&self, t: &mut Node<K, V, E>) {
    (self.relax_op)(t);
    t.sz -= 1;
  }
  #[inline]
  fn relax(&self, t: &mut Node<K, V, E>) {
    (self.relax_op)(t);
    t.sz = 1;
    if let Some(l) = &t.left {
      t.sz += l.sz;
    }
    if let Some(r) = &t.right {
      t.sz += r.sz;
    }
  }

  //returns (nodes with keys less or equal than key, nodes with keys greater than key)
  pub fn split(
    &self,
    t: Option<Box<Node<K, V, E>>>,
    key: K,
  ) -> (Option<Box<Node<K, V, E>>>, Option<Box<Node<K, V, E>>>) {
    if let Some(mut q) = t {
      if key < q.key {
        let l = {
          let u = &mut q;
          let (l, r) = self.split(
            std::mem::replace::<Option<Box<Node<K, V, E>>>>(&mut u.left, None),
            key,
          );
          u.left = r;
          self.relax(u);
          l
        };
        (l, Some(q))
      } else {
        let r = {
          let u = &mut q;
          let (l, r) = self.split(
            std::mem::replace::<Option<Box<Node<K, V, E>>>>(&mut u.right, None),
            key,
          );
          u.right = l;
          self.relax(u);
          r
        };
        (Some(q), r)
      }
    } else {
      (None, None)
    }
  }
  pub fn merge(
    &self,
    l: Option<Box<Node<K, V, E>>>,
    r: Option<Box<Node<K, V, E>>>,
  ) -> Option<Box<Node<K, V, E>>> {
    if l.is_none() {
      return r;
    }
    if r.is_none() {
      return l;
    }
    let mut l = l.unwrap();
    let mut r = r.unwrap();
    if l.y > r.y {
      let w = std::mem::replace(&mut l.right, None);
      l.right = self.merge(w, Some(r));
      self.relax(&mut l);
      Some(l)
    } else {
      let w = std::mem::replace(&mut r.left, None);
      r.left = self.merge(Some(l), w);
      self.relax(&mut r);
      Some(r)
    }
  }
  fn new_node(&self, key: K, y: i32, value: V) -> Node<K, V, E> {
    Node {
      key,
      value,
      y,
      left: None,
      right: None,
      extra: E::default(),
      sz: 1,
    }
  }
  pub fn insert(
    &self,
    t: Option<Box<Node<K, V, E>>>,
    key: K,
    y: i32,
    value: V,
  ) -> Option<Box<Node<K, V, E>>> {
    if t.is_none() {
      return Some(Box::new(self.new_node(key, y, value)));
    }
    let mut t = t.unwrap();
    if y >= t.y {
      let (l, r) = self.split(Some(t), key.clone());
      let mut p = self.new_node(key, y, value);
      p.left = l;
      p.right = r;
      self.relax(&mut p);
      return Some(Box::new(p));
    }
    if key <= t.key {
      let w = self.insert(std::mem::replace(&mut t.left, None), key, y, value);
      t.left = w;
    } else {
      let w = self.insert(std::mem::replace(&mut t.right, None), key, y, value);
      t.right = w;
    }
    self.relax_inc(&mut t);
    Some(t)
  }
  pub fn remove(
    &self,
    t: Option<Box<Node<K, V, E>>>,
    key: &K,
  ) -> (Option<Box<Node<K, V, E>>>, bool) {
    if t.is_none() {
      return (t, false);
    }
    let mut t = t.unwrap();
    if *key < t.key {
      let (w, removed) = self.remove(std::mem::replace(&mut t.left, None), key);
      t.left = w;
      if removed {
        self.relax_dec(&mut t);
      }
      return (Some(t), removed);
    }
    if t.key < *key {
      let (w, removed) = self.remove(std::mem::replace(&mut t.right, None), key);
      t.right = w;
      if removed {
        self.relax_dec(&mut t);
      }
      return (Some(t), removed);
    }
    let l = std::mem::replace(&mut t.left, None);
    let r = std::mem::replace(&mut t.right, None);
    (self.merge(l, r), true)
  }
  pub fn count_less(t: &Option<Box<Node<K, V, E>>>, key: &K) -> usize {
    if let Some(t) = t {
      if t.key < *key {
        Treap::count_less(&t.right, key)
          + (if let Some(l) = &t.left {
            1 + l.sz as usize
          } else {
            1
          })
      } else {
        Treap::count_less(&t.left, key)
      }
    } else {
      0
    }
  }
}
