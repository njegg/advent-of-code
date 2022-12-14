package scala

import file.LoadFile

/* --- Day 7: No Space Left On Device --- */

object D7_1 {
  def main(args: Array[String]): Unit = {
    val root = LoadFile.loadFromFile("7_i.txt")
    root.print(0)

    val ans = root.sumSmall

    println(s"\n$ans")
  }
}
