package d14

class Point(var x: Int, var y: Int) {
  def this(s: String) {
    this(Integer.parseInt(s.split(",")(0)), Integer.parseInt(s.split(",")(1)))
  }

  def set(x :Int, y: Int): Unit = {
    this.x = x
    this.y = y
  }

  override def equals(obj: Any): Boolean = {
    val that = obj.asInstanceOf[Point]
    this.x == that.x && this.y == that.y
  }

  override def toString: String = s"($x,$y)"

  override def hashCode(): Int = (x, y).hashCode()
}
