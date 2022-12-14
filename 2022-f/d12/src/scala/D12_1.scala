package scala

import scala.collection.mutable
import scala.io.Source

object D12_1 {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("12_i.txt")

    val lines = source.getLines().toList
    val h = lines.size
    val w = lines.head.length

    val graph = Array.ofDim[Node](h, w)
    var end: Node = null
    var start: Node = null

    for (i <- 0 until h; j <- 0 until w) {
      val c = lines(i)(j)
      val node: Node = new Node(c)
      if (c == 'E') end = node
      if (c == 'S') start = node
      graph(i)(j) = node
    }
    source.close()

    start.height = 'a'
    start.steps = 0
    end.height = 'z'

    for (i <- 0 until h; j <- 0 until w) {
      val current = graph(i)(j)

      if (i > 0) {
        val neighbour = graph(i - 1)(j)
        if (neighbour.height - current.height < 2) {
          current.neighbours(0) = neighbour
        }
      }

      if (j < w - 1) {
        val neighbour = graph(i)(j + 1)
        if (neighbour.height - current.height < 2)
          current.neighbours(1) = neighbour
      }

      if (i < h - 1) {
        val neighbour = graph(i + 1)(j)
        if (neighbour.height - current.height < 2)
          current.neighbours(2) = neighbour
      }

      if (j > 0) {
        val neighbour = graph(i)(j - 1)
        if (neighbour.height - current.height < 2)
          current.neighbours(3) = neighbour
      }
    }

    val q = mutable.PriorityQueue.empty[Node]
    q.addOne(start)
    start.visited = true
    end.visited = true

    while (q.nonEmpty) {
      val node = q.dequeue()

      node.neighbours.filter(_ != null).foreach(neighbour => {
        val stepsToNeighbour = node.steps + 1
        if (stepsToNeighbour < neighbour.steps) {
          neighbour.steps = stepsToNeighbour
          neighbour.parent = node

          if (!neighbour.visited) {
            q.addOne(neighbour)
            neighbour.visited = true
          }
        }
      })
    }

    println(end.steps)
  }
}
