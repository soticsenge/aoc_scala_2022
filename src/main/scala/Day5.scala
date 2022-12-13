import scala.io.Source

object Helper {
  def inverse[T](l: List[List[T]]): List[List[T]] = {
    l.foldLeft(List.empty[List[T]])((acc: List[List[T]], newList: List[T]) => {
      newList.zipWithIndex.map[List[T]] { case (e, i) => if (acc.isEmpty) List(e) else acc(i) :+ e }
    })
  }
}

case class Movement(move: Int, from: Int, to: Int)

case class DisplayCrates(crates: List[List[String]]) {

  import Helper._

  override def toString: String = {
    val dim = crates.map(_.size).max
    inverse(crates
      .map(list => list ::: List.fill(dim - list.size)(" ")))
      .map(list => list.map(letter => s" [$letter] ")
        .mkString)
      .reverse
      .appended(" 1    2    3    4    5    6    7    8    9")
      .mkString("\n")
  }
}

object Movement {
  def apply(str: String): Movement = {
    str match {
      case s"move $count from $from to $to" => Movement(count.toInt, from.toInt, to.toInt)
      case _ => throw new Exception("bad input")
    }
  }
}

object Day5 extends App {

  import Helper._

  val filename = "/Users/csengesoti/IdeaProjects/aoc_scaéa/src/main/scala/source_crates.txt"
  val bufferedSource = Source.fromFile(filename)
  val lines = bufferedSource.getLines.toList
  val numPattern = "[A-Z]+".r

  def applyStepToCrates(crates: List[List[String]], step: Movement): List[List[String]] = {
    (0 until step.move).foldLeft(crates)((acc: List[List[String]], _: Int) => {
      val elementToMoveO: Option[String] = acc(step.from - 1).lastOption
      elementToMoveO.map(elementToMove => {
        acc.zipWithIndex.map {
          case (e, i) =>
            if (i == step.from - 1) e.dropRight(1)
            else if (i == step.to - 1) e :+ elementToMove
            else e
        }
      }).getOrElse(acc)
    })

  }

  def applyStepToCrates2(crates: List[List[String]], step: Movement): List[List[String]] = {
    val elementsToMove: List[String] = crates(step.from - 1).takeRight(step.move)
    crates.zipWithIndex.map {
      case (e, i) =>
        if (i == step.from - 1) e.dropRight(step.move)
        else if (i == step.to - 1) e ::: elementsToMove
        else e
    }
  }

  val crates = inverse(lines
    .take(lines.indexWhere(_.isEmpty))
    .dropRight(1)
    .reverse
    .map(_.grouped(4)
      .toList
      .map((l: String) =>
        numPattern.findFirstIn(l)
      )
    )).map(l => l.flatten)
  val steps: Seq[Movement] = lines.reverse.take(lines.reverse.indexWhere(_.isEmpty)).map((line: String) => Movement(line))
  val stepsInOrder = steps.reverse
  val finalCrates = stepsInOrder.foldLeft(crates)((acc: List[List[String]], movement: Movement) => {
    println(movement)
    val a = applyStepToCrates2(acc, movement)
    println(DisplayCrates(a))
    applyStepToCrates2(acc, movement)
  })
  println(finalCrates.flatMap(_.lastOption).mkString)

}
