import scala.io.Source

object Day6 extends App {

  val filename = "/Users/csengesoti/IdeaProjects/aoc_scaéa/src/main/scala/source_receiver.txt"
  val bufferedSource = Source.fromFile(filename)
  val lines = bufferedSource.getLines.mkString

  val first = getMarker(lines, 4)
  val second = getMarker(lines, 14)

  private def getMarker(str: String, markerSize: Int) = {
    str.sliding(markerSize).zipWithIndex.flatMap { case (e, i) => if (e.toSet.size == markerSize) Some(i) else None
    }.toList.head + markerSize
  }

  println(first)
  println(second)
}
