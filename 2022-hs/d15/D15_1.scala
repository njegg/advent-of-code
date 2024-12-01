import scala.collection.mutable
import scala.io.Source

object D15_1 {
  class Scanner {
    var px: Int = 0
    var py: Int = 0
    var bx: Int = 0
    var by: Int = 0
    var manhattan: Int = 0

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
    }

    override def toString: String = s"p:($px,$py);b:($bx,$by)"
  }

  def main(args: Array[String]): Unit = {
    val targetY = 2000000;
    val source = Source.fromFile("15_i.txt")
    val scanners = source.getLines.toList.map(new Scanner(_))
    source.close

    val hits: mutable.HashSet[Int] = mutable.HashSet.empty[Int]

    scanners.foreach(s => {
      val xRange = s.manhattan - (s.py - targetY).abs
      if (xRange >= 0) {
        for(x <- s.px - xRange to s.px + xRange) hits.add(x)

        if (s.py == targetY) hits.remove(s.px) // Scanner is already there
        if (s.by == targetY) hits.remove(s.bx) // Beacon is already there
      }
    })

    println(hits.size)
  }
}
