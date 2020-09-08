package com.github.antma.cpalgo

final class Heap[T: reflect.ClassTag](private val n: Int, value: T)(implicit o: math.Ordering[T]) {
  private var size = 0
  private val a    = Array.fill(n)(value)
  private val h    = Array.ofDim[Int](n + 1)
  private val g    = Array.fill(n)(-1)
  private def heapifyFront(k: Int) = {
    val he = h(k)
    @scala.annotation.tailrec
    def f(i: Int): Int = {
      val l = i << 1
      if (l <= size) {
        val j = if (l < size && o.compare(a(h(l + 1)), a(h(l))) < 0) (l + 1) else l
        if (o.compare(a(h(j)), a(he)) < 0) {
          h(i) = h(j)
          g(h(i)) = i
          f(j)
        } else i
      } else i
    }
    val i = f(k)
    if (i != k) {
      h(i) = he
      g(he) = i
    }
  }
  private def heapifyBack(k: Int) = {
    val he = h(k)
    @scala.annotation.tailrec
    def f(i: Int): Int = {
      if (i > 1) {
        val j = i >> 1
        if (o.compare(a(he), a(h(j))) < 0) {
          h(i) = h(j)
          g(h(i)) = i
          f(j)
        } else i
      } else 1
    }
    val i = f(k)
    if (i != k) {
      h(i) = he
      g(he) = i
    }
  }
  private def insert(i: Int) = {
    size += 1
    h(size) = i
    g(i) = size
    heapifyBack(size)
  }
  def contains(k: Int) = g(k) >= 0
  def decreaseKey(k: Int, value: T) = {
    a(k) = value
    val pos = g(k)
    if (pos < 0) {
      insert(k)
    } else {
      heapifyBack(pos)
    }
  }
  def update(k: Int, value: T) = {
    a(k) = value
    val pos = g(k)
    if (pos < 0) {
      insert(k)
    } else {
      val c = o.compare(value, a(k))
      if (c < 0) {
        heapifyBack(pos)
      } else if (c > 0) {
        heapifyFront(pos)
      }
    }
  }
  def extractMin() = {
    assert(size > 0)
    val he = h(1)
    g(he) = -1
    size -= 1
    if (size > 0) {
      h(1) = h(size + 1)
      g(h(1)) = 1
      heapifyFront(1)
    }
    he
  }
  def apply(k: Int) = a(k)
  def nonEmpty      = size > 0
  def isEmpty       = size == 0
}

final class SimpleHeap[T: reflect.ClassTag](h: Array[T])(implicit n: math.Numeric[T]) {
  private[this] var size = h.size
  private[this] def heapifyFront(k: Int) = {
    val he = h(k)
    @scala.annotation.tailrec
    def f(i: Int): Int = {
      val l = (i << 1) + 1
      if (l < size) {
        val j = if (l + 1 < size && n.compare(h(l + 1), h(l)) < 0) l + 1 else l
        if (n.compare(h(j), he) < 0) {
          h(i) = h(j)
          f(j)
        } else i
      } else i
    }
    val i = f(k)
    if (i != k) {
      h(i) = he
    }
  }
  for (i <- (0 to ((size - 2) >> 1)).reverse) heapifyFront(i)
  def extractMin(): T = {
    require(size > 0)
    val he = h(0)
    size -= 1
    if (size > 0) {
      h(0) = h(size)
      heapifyFront(0)
    }
    he
  }
  def nonEmpty = size > 0
  def isEmpty  = size == 0
}
