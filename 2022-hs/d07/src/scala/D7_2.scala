package scala

import file.LoadFile

/* --- Day 7: No Space Left On Device, Part 2 --- */

object D7_2 {
    val SPACE = 70_000_000
    val UPDATE_SIZE = 30_000_000

    def main(args: Array[String]): Unit = {
        val root = LoadFile.loadFromFile("./7_i.txt")

        val USED = root.getSize
        val UNUSED = SPACE - USED
        val NEEDED = UPDATE_SIZE - UNUSED

        root.print(0)

        val del = root.closest(NEEDED)

        println
        println(s"used: $USED/$SPACE")
        println(s"unused: $UNUSED")
        println(s"needed: $NEEDED")
        println(s"delete this: $del")
    }
}
