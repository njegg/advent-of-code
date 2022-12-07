package file

import scala.collection.mutable
import scala.io.Source

class File (var name: String,
            var size: Int,
            var parent: File,
            var dir: Boolean,
            var children: mutable.HashMap[String, File]) {

  def this(name: String, parent: File) =
    this(name, 0, parent, true, new mutable.HashMap[String, File]())

  def this(name: String, size: Int, parent: File) =
    this(name, size, parent, false, new mutable.HashMap[String, File]())

  def print(depth: Int): Unit = {
    println(s"${"  " * depth} - ${toString()}")
    if (dir) children.values.foreach(_.print(depth + 1))
  }

  def getSize: Int = {
    if (dir) size = children.values.toList.map(_.getSize).sum
    size
  }

  def sumSmall: Int = {
    val res = if (dir && size <= 100_000) size else 0
    res + children.values.map(_.sumSmall).sum
  }

  def closest(needed: Int): File = {
    children.values.filter(_.dir).map(_.closest(needed)).fold(this)(_.closer(_, needed))
  }

  def closer(that: File, n: Int): File = {
    if (dif(n) < that.dif(n)) this else that
  }

  def dif(n: Int): Int = {
    val dif = size - n
    if (dif < 0) Int.MaxValue else dif
  }

  override def toString: String = {
    s"$name ${if (dir) s"(dir, size=$size)" else s"(src.file, size=${size.toString})"}"
  }
}

object LoadFile {
  def loadFromFile(path: String): File = {
    val source = Source.fromFile(path)
    val lines = source.getLines().toList
    source.close

    val root = new File("/", null)
    var cur = root
    var name: String = null

    for (line <- lines.drop(2)) {
      val split = line.split(" ")
      name = split(1)

      if (line.startsWith("$ cd")) {
        cur = if (line(5) == '.') cur.parent else cur.children(split(2))
      } else {
        if (line(0) == 'd') {
          cur.children.put(name, new File(name, cur))
        } else if (line(0).isDigit) {
          cur.children.put(name, new File(split(1), split(0).toInt, cur))
        }
      }
    }

    root.getSize
    root
  }
}
