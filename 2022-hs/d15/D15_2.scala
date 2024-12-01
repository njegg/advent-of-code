import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object D15_2 {
  class Scanner {
    var px: Int = 0
    var py: Int = 0
    var bx: Int = 0
    var by: Int = 0
    var manhattan: Int = 0
    var cover: (Int, Int) = _

    def this(string: String) {
      this()
      val split = string
        .filter(c => ",:-".contains(c) || c.isDigit)
        .split(":")
        .map(_
          .split(",")
          .map(Integer.parseInt)
        )

      this.px = split(0)(0)
      this.py = split(0)(1)
      this.bx = split(1)(0)
      this.by = split(1)(1)
      this.manhattan = (px - bx).abs + (py - by).abs
      this.cover = (px - manhattan, px + manhattan)
    }

    def coverY(y: Int): (Int ,Int) = {
      val dif = (py - y).abs
      var cl = cover._1 + dif
      if (cl < 0) cl = 0

      var cr = cover._2 - dif
      if (cr > maxValue) cr = maxValue

      (cl, cr)
    }

    override def toString: String = s"p:($px,$py);b:($bx,$by)"
  }

  val maxValue = 4000000

  def main(args: Array[String]): Unit = {
    val source = Source.fromFile(if (maxValue == 20) "15_s.txt" else "15_i.txt")
    val scanners = source.getLines.toList.map(new Scanner(_))
    source.close

    for (y <- 0 to maxValue) {
      val ranges = mutable.HashSet.empty[(Int, Int)]

      for (s <- scanners) {
        var range = s.coverY(y)

        if (range._1 < range._2) {
          var done = false

          while (!done) {
            val overlap = ranges.find(c => overlaps(c, range)).orNull

            if (overlap == null) {
              ranges.add(range)
              done = true
            } else {
              range = (range._1.min(overlap._1), range._2.max(overlap._2)) // Join
              ranges.remove(overlap)
            }
          }
        }
      }

      if (ranges.size == 2) {
        val c1 = ranges.head
        ranges.remove(c1)
        val c2 = ranges.head
        val x = if (c1._1 < c2._1) c1._2 else c2._2 + 1
        println(x * 4000000L + y)
      }
    }
  }

  def overlaps(a: (Int, Int), b: (Int, Int)): Boolean = {
    a._2 >= b._1 && a._1 <= b._1 || a._1 >= b._1 && a._1 <= b._2 || a._1 == b._2 - 1 || a._2 == b._1 - 1
  }
}
