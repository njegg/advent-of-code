package d13

import scala.collection.mutable.ListBuffer

class Packet(var value: Int) extends Ordered[Packet] {
  var list: ListBuffer[Packet] = ListBuffer.empty[Packet]
  var isList: Boolean = false

  def this() {
    this(-1)
    isList = true
  }

  override def toString: String =
    if (isList) list.mkString("[", ",", "]") else value.toString

           def ==(that: Packet): Boolean = this.compare(that) == 0
  override def  >(that: Packet): Boolean = this.compare(that) > 0
  override def  <(that: Packet): Boolean = this.compare(that) < 0

  override def compare(that: Packet): Int = {
    if (this.isList || that.isList) {
      var l = this
      var r = that

      if (!l.isList) {
        l = new Packet()
        l.list += new Packet(this.value)
      }

      if (!r.isList) {
        r = new Packet()
        r.list += new Packet(that.value)
      }

      l.list.zip(r.list)
        .map(p => p._1.compare(p._2))
        .find(_ != 0)
        .getOrElse(l.list.size - r.list.size)
    } else {
      this.value - that.value
    }
  }
}
