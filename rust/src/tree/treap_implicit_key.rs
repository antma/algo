pub struct INode<V, E> {
  pub value: V,
  pub extra: E,
  left: Option<Box<INode<V, E>>>,
  right: Option<Box<INode<V, E>>>,
  y: i32,
  sz: u32,
}

pub struct ITreapMethods<V, E> {
  push_op: fn(&mut INode<V, E>),
  relax_op: fn(&mut INode<V, E>),
}

impl<V, E: Default> ITreapMethods<V, E> {
  #[inline]
  fn push(&self, p: &mut Option<Box<INode<V, E>>>) {
    if let Some(ref mut q) = p {
      (self.push_op)(q);
    }
  }
  #[inline]
  fn relax_inc(&self, t: &mut INode<V, E>) {
    (self.relax_op)(t);
    t.sz += 1;
  }
  #[inline]
  fn relax(&self, t: &mut INode<V, E>) {
    (self.relax_op)(t);
    t.sz = 1;
    if let Some(l) = &t.left {
      t.sz += l.sz;
    }
    if let Some(r) = &t.right {
      t.sz += r.sz;
    }
  }
  pub fn split(
    &self,
    t: Option<Box<INode<V, E>>>,
    pos: usize,
  ) -> (Option<Box<INode<V, E>>>, Option<Box<INode<V, E>>>) {
    if let Some(mut q) = t {
      let ls = {
        let u = &mut q;
        (self.push_op)(u);
        if let Some(w) = &u.left {
          w.sz as usize
        } else {
          0
        }
      };
      if pos <= ls {
        let l = {
          let u = &mut q;
          let (l, r) = self.split(
            std::mem::replace::<Option<Box<INode<V, E>>>>(&mut u.left, None),
            pos,
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
            std::mem::replace::<Option<Box<INode<V, E>>>>(&mut u.right, None),
            pos - ls - 1,
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
    mut l: Option<Box<INode<V, E>>>,
    mut r: Option<Box<INode<V, E>>>,
  ) -> Option<Box<INode<V, E>>> {
    if l.is_none() {
      self.push(&mut r);
      return r;
    }
    if r.is_none() {
      self.push(&mut l);
      return l;
    }
    let mut l = l.unwrap();
    let mut r = r.unwrap();
    (self.push_op)(&mut l);
    (self.push_op)(&mut r);
    if l.y >= r.y {
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
  fn new_inode(&self, y: i32, value: V) -> INode<V, E> {
    INode {
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
    t: Option<Box<INode<V, E>>>,
    pos: usize,
    y: i32,
    value: V,
  ) -> Option<Box<INode<V, E>>> {
    if t.is_none() {
      return Some(Box::new(self.new_inode(y, value)));
    }
    let mut t = t.unwrap();
    if y >= t.y {
      let (l, r) = self.split(Some(t), pos);
      let mut p = self.new_inode(y, value);
      p.left = l;
      p.right = r;
      self.relax(&mut p);
      return Some(Box::new(p));
    }
    let ls = {
      let u = &mut t;
      (self.push_op)(u);
      if let Some(w) = &u.left {
        w.sz as usize
      } else {
        0
      }
    };
    if pos <= ls {
      let w = self.insert(std::mem::replace(&mut t.left, None), pos, y, value);
      t.left = w;
    } else {
      let w = self.insert(
        std::mem::replace(&mut t.right, None),
        pos - ls - 1,
        y,
        value,
      );
      t.right = w;
    }
    self.relax_inc(&mut t);
    Some(t)
  }
  pub fn get<'a, 'b>(&'a self, t: &'b mut Option<Box<INode<V, E>>>, pos: usize) -> &'b INode<V, E> {
    if let Some(ref mut t) = t {
      (self.push_op)(t);
      let ls = if let Some(l) = &t.left {
        l.sz as usize
      } else {
        0
      };
      if pos < ls {
        return self.get(&mut t.left, pos);
      }
      let pos = pos - ls;
      if pos == 0 {
        t
      } else {
        self.get(&mut t.right, pos - 1)
      }
    } else {
      panic!("get from empty treap");
    }
  }
}
