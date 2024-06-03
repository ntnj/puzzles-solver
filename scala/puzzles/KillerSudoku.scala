package puzzles

import com.microsoft.z3
import z3s.{*, given}
import scala.language.implicitConversions

@main def KillerSudoku(): Unit =
  given ctx: z3.Context = new z3.Context()

  val s = ctx.mkSolver()
  val grid = func[(BV[5], BV[5]), BV[5]]("grid")
  s.add(forall[BV[5], BV[5]]((x, y) => grid(x, y) >= 1 && grid(x, y) <= 9))
  (1 to 9).foreach: i =>
    s.add(distinct((1 to 9).map(grid(i, _))*))
    s.add(distinct((1 to 9).map(grid(_, i))*))

  def cross[T](a: Seq[T], b: Seq[T]): Seq[(T, T)] = for x <- a; y <- b yield (x, y)
  cross((1 to 9).grouped(3).toSeq, (1 to 9).grouped(3).toSeq).foreach: (il, jl) =>
    s.add(distinct(cross(il, jl).map(grid(_, _))*))

  puzzleSudoku.foreach: (sm, is) =>
    val l = is.map(grid(_, _))
    s.add(distinct(l*))
    s.add(sum(l*) === sm)

  println(s)
  s.check() match
    case a @ (z3.Status.UNSATISFIABLE | z3.Status.UNKNOWN) => println(a)
    case z3.Status.SATISFIABLE =>
      (1 to 9).foreach: i =>
        println((1 to 9).map(j => s.getModel.evaluate(grid(i, j), true)).mkString(" "))

private def puzzleSudoku: Seq[(Int, Seq[(Int, Int)])] =
  // https://codegolf.stackexchange.com/questions/10534/build-a-killer-sudoku-solver
  val g = "AABBBCDEFGGHHCCDEFGGIICJKKFLMMINJKOFLPPQNJOORSPTQNUVVRSTTQWUUXXSYZWWaaXXSYZWbbbcc"
  val sums = "3 15 22 4 16 15 25 17 9 8 20 6 14 17 17 13 20 12 27 6 20 6 10 14 8 16 15 13 17"
    .split(" ")
    .map(_.toInt)
  g.zipWithIndex
    .groupMap(a => a._1)(i => (i._2 / 9 + 1, i._2 % 9 + 1))
    .toSeq
    .map(a => (sums(if a._1 <= 'Z' then a._1 - 'A' else a._1 - 'a' + 26), a._2))
