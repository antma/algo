use std::rc::Rc;

pub struct PNode<K, V, E> {
  pub key: K,
  pub value: V,
  pub extra: E,
  left: Option<Rc<PNode<K, V, E>>>,
  right: Option<Rc<PNode<K, V, E>>>,
  y: i32,
  sz: u32,
}

pub type PtrPNode<K, V, E> = Option<Rc<PNode<K, V, E>>>;

pub struct PersistentTreap<K, V, E, F> {
  compute_extra: F,
  phantom: std::marker::PhantomData<(K, V, E)>,
}

#[inline]
fn ptr_clone<K, V, E>(p: &PtrPNode<K, V, E>) -> PtrPNode<K, V, E> {
  p.as_ref().map(Rc::clone)
}

#[inline]
fn ptr_size<K, V, E>(p: &PtrPNode<K, V, E>) -> usize {
  p.as_ref().map_or(0, |p| p.sz as usize)
}

impl<K, V, E> PNode<K, V, E> {
  #[inline]
  fn left_subtree_size(&self) -> usize {
    if let Some(w) = &self.left {
      w.sz as usize
    } else {
      0
    }
  }
}

impl<K, V, E, F> PersistentTreap<K, V, E, F>
where
  K: PartialOrd + Clone,
  V: Clone,
  F: Fn(&K, &V, &PtrPNode<K, V, E>, &PtrPNode<K, V, E>, u32) -> E,
{
  pub fn new(compute_extra: F) -> Self {
    Self {
      compute_extra,
      phantom: std::marker::PhantomData,
    }
  }
  fn new_node(&self, key: K, y: i32, value: V) -> PNode<K, V, E> {
    PNode {
      extra: (self.compute_extra)(&key, &value, &None, &None, 1),
      key,
      value,
      y,
      sz: 1,
      left: None,
      right: None,
    }
  }
  fn relax(
    &self,
    key: &K,
    y: i32,
    value: &V,
    left: PtrPNode<K, V, E>,
    right: PtrPNode<K, V, E>,
  ) -> PNode<K, V, E> {
    let sz = (1 + ptr_size(&left) + ptr_size(&right)) as u32;
    PNode {
      key: key.clone(),
      value: value.clone(),
      extra: (self.compute_extra)(key, value, &left, &right, sz),
      y,
      sz,
      left,
      right,
    }
  }
  pub fn split(&self, root: &PtrPNode<K, V, E>, key: &K) -> (PtrPNode<K, V, E>, PtrPNode<K, V, E>) {
    if let Some(q) = root {
      if *key < q.key {
        let (l, r) = self.split(&q.left, key);
        (
          l,
          Some(Rc::new(self.relax(
            &q.key,
            q.y,
            &q.value,
            r,
            ptr_clone(&q.right),
          ))),
        )
      } else {
        let (l, r) = self.split(&q.right, key);
        (
          Some(Rc::new(self.relax(
            &q.key,
            q.y,
            &q.value,
            ptr_clone(&q.left),
            l,
          ))),
          r,
        )
      }
    } else {
      (None, None)
    }
  }
  pub fn merge(&self, l: &PtrPNode<K, V, E>, r: &PtrPNode<K, V, E>) -> PtrPNode<K, V, E> {
    if l.is_none() {
      return ptr_clone(r);
    }
    if r.is_none() {
      return ptr_clone(l);
    }
    let ll = l.as_ref().unwrap();
    let rr = r.as_ref().unwrap();
    let q = if ll.y >= rr.y {
      self.relax(
        &ll.key,
        ll.y,
        &ll.value,
        ptr_clone(&ll.left),
        self.merge(&ll.right, r),
      )
    } else {
      self.relax(
        &rr.key,
        rr.y,
        &rr.value,
        self.merge(l, &rr.left),
        ptr_clone(&rr.right),
      )
    };
    Some(Rc::new(q))
  }
  pub fn insert(&self, root: &PtrPNode<K, V, E>, key: K, y: i32, value: V) -> PtrPNode<K, V, E> {
    if let Some(t) = root {
      if y > t.y {
        let (l, r) = self.split(root, &key);
        return Some(Rc::new(self.relax(&key, y, &value, l, r)));
      }
      let p = if key < t.key {
        let left = self.insert(&t.left, key, y, value);
        self.relax(&t.key, t.y, &t.value, left, ptr_clone(&t.right))
      } else {
        let right = self.insert(&t.right, key, y, value);
        self.relax(&t.key, t.y, &t.value, ptr_clone(&t.left), right)
      };
      Some(Rc::new(p))
    } else {
      return Some(Rc::new(self.new_node(key, y, value)));
    }
  }
  pub fn remove(&self, root: &PtrPNode<K, V, E>, key: K) -> PtrPNode<K, V, E> {
    if let Some(t) = root {
      if key == t.key {
        self.merge(&t.left, &t.right)
      } else {
        let p = if key < t.key {
          let left = self.remove(&t.left, key);
          self.relax(&t.key, t.y, &t.value, left, ptr_clone(&t.right))
        } else {
          let right = self.remove(&t.right, key);
          self.relax(&t.key, t.y, &t.value, ptr_clone(&t.left), right)
        };
        Some(Rc::new(p))
      }
    } else {
      panic!("removing element from empty tree");
    }
  }
  pub fn index<'a, 'b>(
    &'a self,
    root: &'b PtrPNode<K, V, E>,
    pos: usize,
  ) -> Option<&'b PNode<K, V, E>> {
    if let Some(t) = root {
      let ls = t.left_subtree_size();
      if pos < ls {
        return self.index(&t.left, pos);
      }
      let pos = pos - ls;
      if pos == 0 {
        Some(t)
      } else {
        self.index(&t.right, pos - 1)
      }
    } else {
      None
    }
  }
  pub fn height(&self, root: &PtrPNode<K, V, E>, h: u32) -> u32 {
    if root.is_none() {
      return h;
    }
    let p = root.as_ref().unwrap();
    self
      .height(&p.left, h + 1)
      .max(self.height(&p.right, h + 1))
  }
}
