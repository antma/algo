use std::rc::Rc;

pub struct PINode<V, E> {
  pub value: V,
  pub extra: E,
  left: Option<Rc<PINode<V, E>>>,
  right: Option<Rc<PINode<V, E>>>,
  y: i32,
  sz: u32,
}

pub type PtrPINode<V, E> = Option<Rc<PINode<V, E>>>;

pub struct PersistentImplicitKeyTreap<V, E, F> {
  compute_extra: F,
  phantom: std::marker::PhantomData<(V, E)>,
}

#[inline]
fn ptr_clone<V, E>(p: &PtrPINode<V, E>) -> PtrPINode<V, E> {
  p.as_ref().map(Rc::clone)
}

#[inline]
fn ptr_size<V, E>(p: &PtrPINode<V, E>) -> usize {
  p.as_ref().map_or(0, |p| p.sz as usize)
}

impl<V, E> PINode<V, E> {
  #[inline]
  fn left_subtree_size(&self) -> usize {
    if let Some(w) = &self.left {
      w.sz as usize
    } else {
      0
    }
  }
}

impl<V: Clone, E: Clone, F: Fn(&PtrPINode<V, E>, &PtrPINode<V, E>, &V) -> E>
  PersistentImplicitKeyTreap<V, E, F>
{
  pub fn new(compute_extra: F) -> Self {
    Self {
      compute_extra,
      phantom: std::marker::PhantomData,
    }
  }
  fn new_node(&self, y: i32, value: V) -> PINode<V, E> {
    PINode {
      extra: (self.compute_extra)(&None, &None, &value),
      value: value,
      y,
      sz: 1,
      left: None,
      right: None,
    }
  }
  fn relax(
    &self,
    y: i32,
    value: &V,
    left: PtrPINode<V, E>,
    right: PtrPINode<V, E>,
  ) -> PINode<V, E> {
    PINode {
      value: value.clone(),
      extra: (self.compute_extra)(&left, &right, value),
      y,
      sz: (1 + ptr_size(&left) + ptr_size(&right)) as u32,
      left,
      right,
    }
  }
  pub fn split(&self, root: &PtrPINode<V, E>, pos: usize) -> (PtrPINode<V, E>, PtrPINode<V, E>) {
    if let Some(q) = root {
      let ls = q.left_subtree_size();
      if pos <= ls {
        let (l, r) = self.split(&q.left, pos);
        (
          l,
          Some(Rc::new(self.relax(q.y, &q.value, r, ptr_clone(&q.right)))),
        )
      } else {
        let (l, r) = self.split(&q.right, pos - ls - 1);
        (
          Some(Rc::new(self.relax(q.y, &q.value, ptr_clone(&q.left), l))),
          r,
        )
      }
    } else {
      (None, None)
    }
  }
  pub fn merge(&self, l: PtrPINode<V, E>, r: PtrPINode<V, E>) -> PtrPINode<V, E> {
    if l.is_none() {
      return r;
    }
    if r.is_none() {
      return l;
    }
    let l = l.unwrap();
    let r = r.unwrap();
    let q = if l.y >= r.y {
      self.relax(
        l.y,
        &l.value,
        ptr_clone(&l.left),
        self.merge(ptr_clone(&l.right), Some(r)),
      )
    } else {
      self.relax(
        r.y,
        &r.value,
        self.merge(Some(l), ptr_clone(&r.left)),
        ptr_clone(&r.right),
      )
    };
    Some(Rc::new(q))
  }
  pub fn insert(&self, t: PtrPINode<V, E>, pos: usize, y: i32, value: V) -> PtrPINode<V, E> {
    if let Some(t) = t {
      if y >= t.y {
        let (l, r) = self.split(&Some(t), pos);
        return Some(Rc::new(self.relax(y, &value, l, r)));
      }
      let ls = t.left_subtree_size();
      let p = if pos <= ls {
        let left = self.insert(ptr_clone(&t.left), pos, y, value);
        self.relax(t.y, &t.value, left, ptr_clone(&t.right))
      } else {
        let right = self.insert(ptr_clone(&t.right), pos - ls - 1, y, value);
        self.relax(t.y, &t.value, ptr_clone(&t.left), right)
      };
      Some(Rc::new(p))
    } else {
      return Some(Rc::new(self.new_node(y, value)));
    }
  }
  pub fn get<'a, 'b>(&'a self, t: &'b PtrPINode<V, E>, pos: usize) -> Option<&'b PINode<V, E>> {
    if let Some(t) = t {
      let ls = t.left_subtree_size();
      if pos < ls {
        return self.get(&t.left, pos);
      }
      let pos = pos - ls;
      if pos == 0 {
        Some(t)
      } else {
        self.get(&t.right, pos - 1)
      }
    } else {
      None
    }
  }
  pub fn remove(&self, t: PtrPINode<V, E>, pos: usize) -> PtrPINode<V, E> {
    let t = t.unwrap();
    let ls = t.left_subtree_size();
    if pos == ls {
      self.merge(ptr_clone(&t.left), ptr_clone(&t.right))
    } else {
      let p = if pos < ls {
        let left = self.remove(ptr_clone(&t.left), pos);
        self.relax(t.y, &t.value, left, ptr_clone(&t.right))
      } else {
        let right = self.remove(ptr_clone(&t.right), pos - ls - 1);
        self.relax(t.y, &t.value, ptr_clone(&t.left), right)
      };
      Some(Rc::new(p))
    }
  }
}
