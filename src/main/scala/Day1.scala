import java.util.stream.Collectors
import scala.io.Source

object Day1 extends App {
  val filename = "/Users/csengesoti/IdeaProjects/aoc_scaéa/src/main/scala/source.txt"
  val bufferedSource = Source.fromFile(filename)
  val a = bufferedSource.getLines.foldLeft((Array[Int](),List[Array[Int]]()))(
      (acc, line: String) => {
        line match {
          case "" => (Array(),acc._1 :: acc._2)
          case _: String => ((acc._1 :+ line.toInt),acc._2)
        }
      }
    )._2.reverse
  //.map(a =>a.max).max
  println(a.map(a => a.sum).max)
  println(a.map(a => a.sum).sortWith(_ > _).splitAt(3)._1.sum)
  }
