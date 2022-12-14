package d13

import scala.io.Source

object D13_1 {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("2022-hs/d13/13_i.txt")
    val lineIt = source.getLines.toList.filter(!_.isBlank).iterator
    source.close

    var index: Int = 1
    var indices: Int = 0

    while (lineIt.hasNext) {
      val l = readPacket(lineIt.next.iterator)
      val r = readPacket(lineIt.next.iterator)

      if (l < r) indices += index

      index += 1
    }

    println(indices)
  }

  def readPacket(it: Iterator[Char]): Packet = {
    it.next()
    readPacketHelper(it)
  }

  def readPacketHelper(it: Iterator[Char]): Packet = {
    val packet: Packet = new Packet()
    var c = it.next()

    while (true) {
      if (c == '[') {
        packet.list += readPacketHelper(it)
        c = it.next()
      } else if (c == ']') {
        return packet
      } else if (c.isDigit) {
        var value = 0

        while (c.isDigit) {
          value *= 10
          value += c - '0'
          c = it.next()
        }

        packet.list += new Packet(value)
      } else {
        c = it.next()
      }
    }

    packet
  }
}
