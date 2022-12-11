import scala.io.Source

object Rucksack extends App {
  val filename = "/Users/csengesoti/IdeaProjects/aoc_scaéa/src/main/scala/source_rs.txt"
  val bufferedSource = Source.fromFile(filename)
  // val a = bufferedSource.getLines.map(line => line.splitAt(line.length/2)).map(a=> a._1.toSet.intersect(a._2.toSet).head).map(getNumber).sum
  val b = bufferedSource.getLines.grouped(3).map((group: Seq[String]) => group.map(_.toSet)).map(e => e.foldLeft(Set.empty[Char])((acc: Set[Char], array: Set[Char]) => {
    acc.size match {
      case 0 => array
      case _ => acc.intersect(array)
    }
  }))
  private def getNumber(a: Char): Int = {
    val b = if (a.toInt > 96) a.toInt - 96 else a.toInt - 64 + 26
    b
  }
  println(b.map(e => getNumber(e.head)).sum)
  //.map(a =>a.max).max
  //println(a.map(a => a.sum).max)
  //println(a.map(a => a.sum).sortWith(_ > _).splitAt(3)._1.sum)
  }
