import java.util.Random

typealias TreapMapKey = Int
typealias TreapMapValue = Int
typealias TreapMapExtra = Long

fun _size(t: Node?):Int = if (t == null) 0 else t.sz
fun _extra(t: Node?): TreapMapExtra = if (t == null) 0L else t.e

class Node(val x: TreapMapKey, val y: Int, val value: TreapMapValue, val left: Node?, val right: Node?) {
  val sz = 1 + _size(left) + _size(right)
  val e = value + _extra(left) + _extra(right)
}

fun split (t: Node?, x: TreapMapKey):Pair<Node?, Node?> {
  if (t == null) return Pair(null, null)
  if (x < t.x) {
    val (l, r) = split(t.left, x)
    return Pair(l, Node(t.x, t.y, t.value, r, t.right))
  } else {
    val (l, r) = split(t.right, x)
    return Pair(Node(t.x, t.y, t.value, t.left, l), r)
  }
}

fun merge(l: Node?, r: Node?): Node? {
  if (l == null) return r
  if (r == null) return l
  if (l.y > r.y) {
    return Node(l.x, l.y, l.value, l.left, merge(l.right, r))
  } else {
    return Node(r.x, r.y, r.value, merge(l, r.left), r.right)
  }
}

fun insert(t: Node?, p: Node): Node {
  if (t == null) {
    return p
  }
  if (t.y >= p.y) {
    if (p.x < t.x) {
      return Node(t.x, t.y, t.value, insert(t.left, p), t.right)
    } else {
      return Node(t.x, t.y, t.value, t.left, insert(t.right, p))
    }
  }
  val (l, r) = split (t, p.x);
  return Node(p.x, p.y, p.value, l, r)
}
