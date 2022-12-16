import scala.io.Source

object Day9 extends App {

  import Math._

  case class Movement(dir: String, dis: Int)

  object Movement {
    def apply(str: String): Movement = {
      str match {
        case s"$dir $dis" => Movement(dir, dis.toInt)
        case _ => throw new Exception("bad input")
      }
    }
  }

  case class Position(x: Int, y: Int)

  case class TableState(hPos: Position, tPos: Position, visited: Set[Position]) {
    override def toString: String = {
      val dim = Seq(10, tPos.x + 1, tPos.y + 1, hPos.x + 1, hPos.y + 1).max
      List.fill(dim)(List.fill(dim)(" . "))
        .zipWithIndex
        .map {
          case (e, i) =>
            if (tPos.y == i) e.updated(tPos.x, " T ") else e
        }
        .zipWithIndex
        .map {
          case (e, i) =>
            if (hPos.y == i) e.updated(hPos.x, " H ") else e
        }
        .map(_.mkString)
        .reverse.mkString("\n").appendedAll("\n" + hPos.toString + tPos.toString + visited.size)
    }
  }

  def applyMove(state: TableState, movement: Movement): TableState = {
    (0 until movement.dis).foldLeft(state)((acc: TableState, _: Int) => {
      val tmpState = movement.dir match {
        case "U" => TableState(Position(acc.hPos.x, acc.hPos.y + 1), acc.tPos, acc.visited)
        case "D" => TableState(Position(acc.hPos.x, acc.hPos.y - 1), acc.tPos, acc.visited)
        case "R" => TableState(Position(acc.hPos.x + 1, acc.hPos.y), acc.tPos, acc.visited)
        case "L" => TableState(Position(acc.hPos.x - 1, acc.hPos.y), acc.tPos, acc.visited)
      }
      println("headmovement", movement)
      println(tmpState)
      println("tailmovement")
      val finalState = applyTailMove(tmpState)

      println(finalState)
      finalState
    })
  }

  def applyTailMove(state: TableState): TableState = {
    if (sqrt(pow(abs(state.hPos.x - state.tPos.x), 2) + pow(abs(state.hPos.y - state.tPos.y), 2)) > sqrt(2)) {
      (state.hPos.x - state.tPos.x, state.hPos.y - state.tPos.y) match {
        case (xDis: Int, yDis: Int) => {
          val newTailPosition = Position(x = state.tPos.x + (Math.min(abs(xDis), 1) * signum(xDis)).toInt, y = state.tPos.y + (Math.min(abs(yDis), 1) * signum(yDis).toInt))
          TableState(state.hPos, newTailPosition, state.visited + newTailPosition )
        }
      }
    } else state
  }

  val filename = "/Users/csengesoti/IdeaProjects/aoc_scaéa/src/main/scala/source_rope.txt"
  val bufferedSource = Source.fromFile(filename)
  val lines = bufferedSource.getLines.toList

  val moves = lines.map((str: String) => Movement(str))
  private val defState = TableState(Position(0, 0), Position(0, 0), Set(Position(0, 0)))
  val finalState = moves.foldLeft(defState)((acc: TableState, movement: Movement) => {
    println(movement)
    val a = applyMove(acc, movement)
    println(a)
    a
  })

}
