package scala

import scala.collection.mutable
import scala.io.Source

object D12_2 {
  def resetNodes(g: Array[Array[Node]]): Unit = {
    g.foreach(a => a.foreach(n => {
      n.steps = Int.MaxValue
      n.visited = false
    }))
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("12_i.txt")

    val lines = source.getLines().toList
    val h = lines.size
    val w = lines.head.length

    val graph = Array.ofDim[Node](h, w)
    var end: Node = null
    var starts: List[(Int, Int)] = List.empty[(Int, Int)]

    for (i <- 0 until h; j <- 0 until w) {
      val c = lines(i)(j)
      val node: Node = new Node(c)

      if (c == 'E')
        end = node
      else if (c == 'S' || c == 'a') {
        starts = starts.appended((i, j))
        node.height = 'a'
      }

      graph(i)(j) = node
    }
    source.close()

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

    var paths: List[Int] = List.empty[Int]
    val q = mutable.PriorityQueue.empty[Node]

    for (start <- starts) {
      resetNodes(graph)

      val startNode = graph(start._1)(start._2)
      startNode.height = 'a'
      startNode.steps = 0
      startNode.visited = true

      q.addOne(startNode)

      while (q.nonEmpty) {
        val node = q.dequeue()
        node.visited = true

        node.neighbours.filter(_ != null).foreach(neighbour => {
          val stepsToNeighbour = node.steps + 1
          if (stepsToNeighbour < neighbour.steps) {
            neighbour.steps = stepsToNeighbour
            neighbour.parent = node

            if (!neighbour.visited) {
              q.addOne(neighbour)
            }
          }
        })
      }

      paths = paths.appended(end.steps)
    }

    println(paths.min)
  }
}
