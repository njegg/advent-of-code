package scala

class Node(var height: Char) extends Ordered[Node] {
  var neighbours: Array[Node] = new Array[Node](4)
  var parent: Node = _
  var steps: Int = Int.MaxValue
  var visited: Boolean = false

  override def toString: String = height.toString
  override def compare(that: Node): Int = that.steps - this.steps
}
