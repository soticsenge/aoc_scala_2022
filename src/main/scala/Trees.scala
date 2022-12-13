import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.io.Source

object Trees extends App {
  val filename = "/Users/csengesoti/IdeaProjects/aoc_scaéa/src/main/scala/source_trees.txt"
  val bufferedSource = Source.fromFile(filename)
  val lines = bufferedSource.getLines.toList

  def inverse[T](l: List[List[T]]): List[List[T]] = {
    l.foldLeft(List.empty[List[T]])((acc: List[List[T]], newList: List[T]) => {
      newList.zipWithIndex.map[List[T]] { case (e, i) => if (acc.isEmpty) List(e) else acc(i) :+ e }
    })
  }

  private def getVisibility2(l: List[List[Int]]): List[List[Boolean]] = {
    l.map(calcVisibility)
  }

  private def calcScenicScore(l: List[List[Int]]): List[List[(Int, Int)]] = {
    l.map(calcScenicScore)
  }

  val calcVisibility = (line: List[Int]) =>
    line
      .zipWithIndex.map({ case (e, i) => {
      line.slice(0, i).reduceOption(_ max _).getOrElse(-1) < e || line.slice(i + 1, line.length).reduceOption(_ max _).getOrElse(-1) < e
    }
    })

  val calcScenicScore = (line: List[Int]) => {
    println(line)
    val a = line
      .zipWithIndex.map({ case (e, i) => {
      val getScenicPoint = (l: List[Int]) => if (l.exists(_ >= e)) l.takeWhile(_ < e).size + 1 else l.takeWhile(_ < e).size
      (getScenicPoint(line.slice(0, i).reverse), getScenicPoint(line.slice(i + 1, line.length)))}
    })
    println(a)
    a
  }

  val a: List[List[Int]] = lines.map(l => {
    l.map(_.toString.toInt).toList
  })
  val b = inverse[Int](a)

  private val horVisibility: List[List[Boolean]] = getVisibility2(a)
  private val horScenicScore: List[List[(Int, Int)]] = calcScenicScore(a)
  private val verticalVisibility: List[List[Boolean]] = getVisibility2(b)
  private val verticalScenicScore: List[List[(Int, Int)]] = calcScenicScore(b)
  println(
    verticalVisibility
      .zipWithIndex.flatMap[Boolean] { case (elements, i) =>
      elements.zipWithIndex.map { case (e, j) =>
        e || horVisibility(j)(i)
      }
    }.count(x => x))

  println(
    verticalScenicScore
      .zipWithIndex.flatMap { case (elements, i) =>
      elements.zipWithIndex.map { case (e, j) =>
        val hor = horScenicScore(j)(i)
        e._1 * e._2 * hor._2 * hor._1
      }
    }.max)
}
