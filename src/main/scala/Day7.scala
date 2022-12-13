import scala.annotation.tailrec
import scala.collection.mutable._
import scala.io.Source

case class Tree(var files: ListBuffer[File], children: ListBuffer[Tree], dirName: String, parent: () => Option[Tree]) {
  def addFile(name: String, size: Int): Unit = {
    files.addOne(File(size = size, name = name))
  }

  def addDir(name: String): Unit = {
    children.addOne(Tree(ListBuffer.empty[File], ListBuffer.empty[Tree], name, () => Some(this)))
  }

  def calculateSize(): (Int, List[Int]) = {
    val w = children.map(_.calculateSize()).toList
    val c: Int = files.map(_.size).sum + w.map(_._1).sum
    (c, w.flatMap(_._2) :+ c)
  }

  def calculateSize2(): (Int, List[Int]) = {
    val w = map2(children.toList)((t: Tree) => t.calculateSize())
    val c: Int = files.map(_.size).sum + w.map(_._1).sum
    (c, w.flatMap(_._2) :+ c)
  }

  def map2[A, B](list: List[A])(function: (A) => B): List[B] = {
    @tailrec
    def loop(rem: List[A], acc: List[B]): List[B] = rem match {
      case Nil => acc.reverse
      case head :: tail => loop(tail, function(head) :: acc)
    }
    loop(list, Nil)
  }
}

case class File(size: Int, name: String)

case class SumWithDirList(sum: Int, dirlist: List[Int]) {
  def ::(b: SumWithDirList): SumWithDirList = {
    SumWithDirList(sum + b.sum, b.dirlist ::: List(dirlist.sum + sum))
  }
}

object Day7 extends App {
  val filename = "/Users/csengesoti/IdeaProjects/aoc_scaéa/src/main/scala/source_files.txt"
  val bufferedSource = Source.fromFile(filename)
  val b = bufferedSource.getLines.toList
  val tree = Tree(ListBuffer.empty, ListBuffer.empty, "/", () => None)

  b.foldLeft(tree)((a: Tree, newLine: String) => {
    newLine match {
      case s"$$ cd /" => a
      case s"$$ cd .." => a.parent().head
      case s"$$ cd $rest" => a.children.find(_.dirName == rest).head
      case s"$$ ls" => a
      case s"dir $rest" => {
        a.addDir(rest)
        a
      }
      case s"$size $file" => {
        a.addFile(file, size.toInt)
        a
      }
      case _ => a
    }
  })

  val a = System.currentTimeMillis()
  val result = tree.calculateSize()
  val bv = System.currentTimeMillis()
  println(a-bv)

  val remainingSpace = 30000000 - (70000000 - result._1)
  println(result._1, remainingSpace, result._2.sorted.reverse)
  println(result._2.filter(_ <= 100000).sorted.reverse.sum)
  println(result._2.filter(_ >= remainingSpace).min)
}
