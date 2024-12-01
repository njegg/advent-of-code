import d14.Point

import scala.io.Source

object D14_1 {
  val ROCK = '#'
  val SAND = 'o'
  val AIR = '.'
  val SAND_SOURCE = '+'
  val NORMAL_C = "\u001B[37m"
  val SAND_C = "\u001B[33m"
  val AIR_C = "\u001B[36m"

  def main(args: Array[String]): Unit = {
    cls()

    val input = Source.fromFile("14_s.txt")
//    val input = Source.fromFile("2022-f/d14/14_s.txt")
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
    val source = new Point(500 - minX, minY)

    rocks = rocks.map(_.map(r => { r.x -= minX; r}))

    val h = maxY - minY + 1
    val w = maxX - minX + 1
    val cave = Array.ofDim[Char](h, w)

    for (y <- 0 until h; x <- 0 until  w) cave(y)(x) = AIR
    cave(source.x)(source.y) = SAND_SOURCE

    for (line <- rocks) {
      var start = line(0)
      for (end <- line) {
        val startX = start.x.min(end.x)
        val startY = start.y.min(end.y)
        val endX = start.x.max(end.x)
        val endY = start.y.max(end.y)

        for (y <- startY to endY; x <- startX to endX)
          cave(y)(x) = ROCK

        start = end
      }
    }

    var desert = 0
    while (dropSand(cave, source)) {
      desert += 1
    }

    printCave(cave)

    println
    println(desert)
  }

  def dropSand(cave: Array[Array[Char]], source: Point): Boolean = {
    var sand = source
    var isMoving = true

    while (isMoving) {
      val movedSand = tryToFall(cave, sand)

      if (movedSand == null) return false
      isMoving = moved(movedSand, sand)

      putToCave(cave, sand, AIR)
      putToCave(cave, movedSand, SAND)
      printCave(cave)

      sand = movedSand
    }

    putToCave(cave, sand, SAND)
    true
  }

  def putToCave(cave: Array[Array[Char]], p: Point, c: Char): Unit =
    cave(p.y)(p.x) = c

  def moved(sand1: Point, sand2: Point): Boolean = {
    sand1.x != sand2.x || sand1.y != sand2.y
  }

  def tryToFall(cave: Array[Array[Char]], sand: Point): Point = {
    val x = sand.x
    val y = sand.y

    // Down
    if (y == cave.length - 1) return null  // Bottom edge + no wall = falls to abyss
    if (cave(y + 1)(x) == AIR) return new Point(x, y + 1) // Can fall

    // Left
    if (x == 0) return null // Left edge, falls to abyss
    if (cave(y + 1)(x - 1) == AIR) return new Point(x - 1, y + 1)

    // Right
    if (x == cave(0).length - 1) return null // Edge
    if (cave(y + 1)(x + 1) == AIR) return new Point(x + 1, y + 1)

    sand // Cant go anywhere, land
  }

  def cls(): Unit = System.out.print("\033[H\033[2J");

  def pointStr(p: Point): String = s"(${p.x},${p.y})"

  def printCave(c: Array[Array[Char]]): Unit = {
    cls()
    var console: String = ""

    c.foreach(l => {
      l.foreach(c => {
        if (c == AIR) console += s"${AIR_C}$c${NORMAL_C}"
        else if (c == SAND) console += s"${SAND_C}$c${NORMAL_C}"
        else console += s"${NORMAL_C}$c${NORMAL_C}"
      })
      console += '\n'
    })

    println(console)
    Thread.sleep(10)
  }
}
