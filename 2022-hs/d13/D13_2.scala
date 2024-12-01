package d13

import scala.io.Source

object D13_2 {
  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("2022-hs/d13/13_i.txt")
    val lines = source.getLines.toList.filter(!_.isBlank)
    source.close

    val divider2 = readPacket("[[2]]".iterator)
    val divider6 = readPacket("[[6]]".iterator)

    val decoderKey = lines
      .map(l => readPacket(l.iterator))
      .appended(divider2)
      .appended(divider6)
      .sorted
      .zipWithIndex
      .filter(p =>
              p._1.toString.equals(divider2.toString)
           || p._1.toString.equals(divider6.toString))
      .map(_._2 + 1)
      .product

    println(decoderKey)
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
