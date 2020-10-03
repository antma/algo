use std::cmp::Ordering;
struct Node<T> {
  key: T,
  left: usize,
  right: usize,
  parent: usize,
}

impl<T: Default> Default for Node<T> {
  fn default() -> Self {
    Self {
      key: Default::default(),
      left: 0,
      right: 0,
      parent: 0,
    }
  }
}

pub struct SplayTree<T> {
  nodes: Vec<Node<T>>,
  root: usize,
}

impl<T: Ord + Default> SplayTree<T> {
  pub fn new() -> Self {
    Self {
      nodes: vec![Default::default()],
      root: 0,
    }
  }
  fn set_parent(&mut self, child: usize, parent: usize) {
    if child != 0 {
      self.nodes[child].parent = parent;
    }
  }
  fn keep_parent(&mut self, v: usize) {
    self.set_parent(self.nodes[v].left, v);
    self.set_parent(self.nodes[v].right, v);
  }
  fn rotate(&mut self, parent: usize, child: usize) {
    let dad = self.nodes[parent].parent;
    if dad != 0 {
      if self.nodes[dad].left == parent {
        self.nodes[dad].left = child;
      } else {
        self.nodes[dad].right = child;
      }
    }
    if self.nodes[parent].left == child {
      self.nodes[parent].left = self.nodes[child].right;
      self.nodes[child].right = parent;
    } else {
      self.nodes[parent].right = self.nodes[child].left;
      self.nodes[child].left = parent;
    }
    self.keep_parent(child);
    self.keep_parent(parent);
    self.nodes[child].parent = dad;
  }
  fn splay(&mut self, v: usize) -> usize {
    if self.nodes[v].parent == 0 {
      return v;
    }
    let parent = self.nodes[v].parent;
    let dad = self.nodes[parent].parent;
    if dad == 0 {
      self.rotate(parent, v);
      return v;
    }
    if (self.nodes[dad].left == parent) == (self.nodes[parent].left == v) {
      //zigzig
      self.rotate(dad, parent);
      self.rotate(parent, v);
    } else {
      self.rotate(parent, v);
      self.rotate(dad, v);
    }
    self.splay(v)
  }
  pub fn find(&mut self, v: usize, key: &T) -> usize {
    if v == 0 {
      return 0;
    }
    match key.cmp(&self.nodes[v].key) {
      Ordering::Equal => self.splay(v),
      Ordering::Less => {
        if self.nodes[v].left != 0 {
          self.find(self.nodes[v].left, key)
        } else {
          self.splay(v)
        }
      }
      Ordering::Greater => {
        if self.nodes[v].right != 0 {
          self.find(self.nodes[v].right, key)
        } else {
          self.splay(v)
        }
      }
    }
  }
  fn split(&mut self, root: usize, key: &T) -> (usize, usize) {
    if root == 0 {
      return (0, 0);
    }
    let root = self.find(root, key);
    match self.nodes[root].key.cmp(key) {
      Ordering::Equal => {
        self.set_parent(self.nodes[root].left, 0);
        self.set_parent(self.nodes[root].right, 0);
        (self.nodes[root].left, self.nodes[root].right)
      }
      Ordering::Less => {
        let right = self.nodes[root].right;
        self.nodes[root].right = 0;
        self.set_parent(right, 0);
        (root, right)
      }
      Ordering::Greater => {
        let left = self.nodes[root].left;
        self.nodes[root].left = 0;
        self.set_parent(left, 0);
        (left, root)
      }
    }
  }
  fn _insert(&mut self, root: usize, key: T) -> usize {
    let (left, right) = self.split(root, &key);
    let root = self.nodes.len();
    self.nodes.push(Node {
      key,
      left,
      right,
      parent: 0,
    });
    self.keep_parent(root);
    root
  }
  fn _count(&self, root: usize) -> usize {
    if root == 0 {
      0
    } else {
      self._count(self.nodes[root].left) + self._count(self.nodes[root].right) + 1
    }
  }
  pub fn insert(&mut self, key: T) {
    self.root = self._insert(self.root, key);
  }
  pub fn contains(&mut self, key: &T) -> bool {
    self.root = self.find(self.root, key);
    self.root != 0 && self.nodes[self.root].key == *key
  }
  pub fn count(&self) -> usize {
    self._count(self.root)
  }
}

impl<T: Ord + Default + Clone> SplayTree<T> {
  fn merge(&mut self, left: usize, right: usize) -> usize {
    if right == 0 {
      return left;
    }
    if left == 0 {
      return right;
    }
    let right = self.find(right, &self.nodes[left].key.clone());
    self.nodes[right].left = left;
    self.nodes[left].parent = right;
    right
  }
  pub fn remove(&mut self, key: &T) {
    let root = self.find(self.root, key);
    let left = self.nodes[root].left;
    let right = self.nodes[root].right;
    self.set_parent(left, 0);
    self.set_parent(right, 0);
    self.root = self.merge(left, right);
  }
}
