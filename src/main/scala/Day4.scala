import scala.io.Source

case class Range(lowerLimit: Int, upperLimit: Int) {
  def includes(other: Range): Boolean = {
    other.lowerLimit <= lowerLimit && other.upperLimit >= upperLimit
  }

  def overlaps(other: Range): Boolean = {
    (this.lowerLimit to this.upperLimit).toSet.intersect((other.lowerLimit to other.upperLimit).toSet).nonEmpty
  }

  def overlaps2(other: Range): Boolean = {
    def predicate = (a: Range, b: Range) => {
      (a.upperLimit >= b.lowerLimit) && (a.lowerLimit <= b.upperLimit)
    }

    predicate(other, this) ||
      predicate(this, other)
  }
}

object Range {
  def fromString(str: String): (Range, Range) =
    toTuple[Range](str.split(',').map(rangeFromNumbers))

  private def rangeFromNumbers(rangeString: String): Range = {
    toRange(rangeString.split('-').map(s => s.toInt))
  }

  def toTuple[B](a: Array[B]): (B, B) = {
    a match {
      case Array(a, b) => (a, b)
    }
  }

  def toRange(a: IndexedSeq[Int]): Range = {
    a match {
      case IndexedSeq(a: Int, b: Int) => Range(a, b)
    }
  }
}

object Day4 extends App {
  val filename = "/Users/csengesoti/IdeaProjects/aoc_scaéa/src/main/scala/source_cleanup.txt"
  val bufferedSource = Source.fromFile(filename)
  val b = bufferedSource.getLines.toList
  val c = b.map(Range.fromString)
  val d = c.count(t => t._1.includes(t._2) || t._2.includes(t._1))
  val e = c.count(t => t._1.overlaps(t._2))
  val f = c.count(t => t._1.overlaps2(t._2))

  println(e)
  println(f)
  println(d)
}
