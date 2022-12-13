import scala.io.Source

object Day2 extends App {

  def calcPoints(ABC: Char, XYZ: Char): Int = {
    // Z beats A
    // X beats A
    // Z beats B
    // Y beats C
    val a = "ABC".toCharArray
    val b = "XYZ".toCharArray

    val n = b.indexOf(XYZ) - a.indexOf(ABC) match {
      case 0 => 3
      case x if (x ==1 | x == -2) => 6
      case x if (x == -1 | x == 2) => 0
    }
   // println(ABC, XYZ, b.indexOf(XYZ)- a.indexOf(ABC), n)
    n + b.indexOf(XYZ) + 1
  }

  def getNextDraw(ABC: Char, XYZ: Char): Char = {
    // Z beats A
    // X beats A
    // Z beats B
    // Y beats C
    val a = "ABC".toCharArray
    val b = "XYZ".toCharArray

    val c = XYZ match {
      case 'X' if a.indexOf(ABC) == 0 => b(2)
      case 'X' if a.indexOf(ABC) != 0 => b(a.indexOf(ABC) - 1)
      case 'Y' => b(a.indexOf(ABC))
      case 'Z' if a.indexOf(ABC) == 2 => b(0)
      case 'Z' if a.indexOf(ABC) != 2 => b(a.indexOf(ABC) + 1)
    }
    println(ABC, XYZ, c, calcPoints(ABC, c))

    c
  }

  val filename = "/Users/csengesoti/IdeaProjects/aoc_scaéa/src/main/scala/source_rps.txt"
  val bufferedSource = Source.fromFile(filename)
  val a = bufferedSource.getLines.toList.map(l => calcPoints(l.toCharArray()(0), getNextDraw(l.toCharArray()(0), l.toCharArray()(2))))
  println(a.sum)
}
