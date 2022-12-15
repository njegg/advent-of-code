import d14.Point

import java.awt.PointerInfo
import scala.collection.mutable
import scala.io.Source

object D14_2 {
  val ROCK = '#'
  val SAND = 'o'
  val AIR = '.'
  val SAND_SOURCE = '+'
  val NORMAL_C = "\u001B[37m"
  val SAND_C = "\u001B[33m"
  val AIR_C = "\u001B[36m"

  def main(args: Array[String]): Unit = {
    cls()

    val input = Source.fromFile("14_i.txt")
//        val input = Source.fromFile("2022-f/d14/14_i.txt")
    var rocks = input.getLines.toList
      .map(_
        .split(" ")
        .filter(!_.equals("->"))
        .map(new Point(_))
      )
    input.close

    val maxX = rocks.map(_.maxBy(_.x)).maxBy(_.x).x
    val maxY = rocks.map(_.maxBy(_.y)).maxBy(_.y).y
    val minX = rocks.map(_.minBy(_.x)).minBy(_.x).x
    val minY = 0

    var h = maxY - minY + 2
    var w = maxX - minX

    rocks = rocks.map(_.map(r => { r.x = r.x - minX + w*2; r}))
    val source = new Point(500 - minX + w*2, minY)

    w = w * 5 + 1
    h += 1
    val cave = Array.ofDim[Char](h, w)

    for (y <- 0 until h; x <- 0 until  w) cave(y)(x) = AIR
    cave(source.y)(source.x) = SAND_SOURCE
    val solids = mutable.HashSet.empty[Point]

    for (line <- rocks) {
      var start = line(0)
      for (end <- line) {
        val startX = start.x.min(end.x)
        val startY = start.y.min(end.y)
        val endX = start.x.max(end.x)
        val endY = start.y.max(end.y)

        for (y <- startY to endY; x <- startX to endX) {
          cave(y)(x) = ROCK
          solids.add(new Point(x, y))
        }

        start = end
      }
    }

    var desert = 1
    while (dropSand(solids, cave, source)) {
      desert += 1
    }

    printCave(cave)

    println
    println(desert)
  }

  def dropSand(solids: mutable.HashSet[Point], cave: Array[Array[Char]], source: Point): Boolean = {
    var sand = source
    var isMoving = true

    while (isMoving) {
      val movedSand = tryToFall(solids, cave, sand)

      if (movedSand == null) return false
      isMoving = moved(movedSand, sand)

      putToCave(cave, sand, AIR)
      putToCave(cave, movedSand, SAND)
//      printCave(cave)

      sand = movedSand
    }

    putToCave(cave, sand, SAND)
    solids.add(sand)
//    printCave(cave)

    !sand.equals(source)
  }

  def putToCave(cave: Array[Array[Char]], p: Point, c: Char): Unit =
    cave(p.y)(p.x) = c

  def moved(sand1: Point, sand2: Point): Boolean = {
    sand1.x != sand2.x || sand1.y != sand2.y
  }

  def tryToFall(solids: mutable.HashSet[Point], cave: Array[Array[Char]], sand: Point): Point = {
    val x = sand.x
    val y = sand.y
    val bottom = cave.length - 1;

    // Down
    val move = new Point(x, y + 1)
    if (y + 1 == bottom) return sand
    if (!solids.contains(move)) return move

    // Left
    move.set(x - 1, y + 1)
    if (y + 1 == bottom || !solids.contains(move)) return move

    // Right
    move.set(x + 1, y + 1)
    if (y + 1 == bottom || !solids.contains(move)) return move

    sand // Cant go anywhere, land
  }

  def cls(): Unit = System.out.print("\033[H\033[2J");

  def pointStr(p: Point): String = s"(${p.x},${p.y})"

  def printCave(cave: Array[Array[Char]]): Unit = {
    cls()
    var console: String = ""

    for (y <- cave.indices; x <- cave(0).indices) {
      val c = cave(y)(x)
      if (y < cave.length - 1) {
        if (c == AIR) console += s"${AIR_C}$c${NORMAL_C}"
        else if (c == SAND) console += s"${SAND_C}$c${NORMAL_C}"
        else console += s"${NORMAL_C}$c${NORMAL_C}"
      } else {
        console += s"${NORMAL_C}$ROCK${NORMAL_C}"
      }

      if (x == cave(0).length - 1) {
        console += '\n'
      }
    }

    println(console)
    Thread.sleep(50)
  }
}
