package puzzles

import com.microsoft.z3
import z3s.{*, given}
import scala.language.implicitConversions

@main def Unequal(): Unit =
  given ctx: z3.Context = new z3.Context()

  val s = ctx.mkSolver()
  val grid = func[(Int, Int), Int]("grid")

  val (size, g) = puzzle
  s.add(forall[Int, Int]((x, y) => grid(x, y) >= 1 && grid(x, y) <= size))
  (1 to size).foreach: i =>
    s.add(distinct((1 to size).map(grid(i, _))*))
    s.add(distinct((1 to size).map(grid(_, i))*))
  g.zipWithIndex.foreach { case ((n, ds), i) =>
    if n > 0 then s.add(grid(i / size + 1, i % size + 1) === n)
    ds.foreach: c =>
      val eq = c match
        case 'U' => grid(i / size + 1, i % size + 1) > grid(i / size, i % size + 1)
        case 'D' => grid(i / size + 1, i % size + 1) > grid(i / size + 2, i % size + 1)
        case 'L' => grid(i / size + 1, i % size + 1) > grid(i / size + 1, i % size)
        case 'R' => grid(i / size + 1, i % size + 1) > grid(i / size + 1, i % size + 2)
      s.add(eq)
  }

  println(s)
  s.check() match
    case a @ (z3.Status.UNSATISFIABLE | z3.Status.UNKNOWN) => println(a)
    case z3.Status.SATISFIABLE =>
      (1 to size).foreach: i =>
        println((1 to size).map(j => s.getModel.evaluate(grid(i, j), true)).mkString(" "))

private def puzzle: (Int, Array[(Int, String)]) =
  val size = 9
  val g =
    "0,0,0,0,0,0,0,0R,0,0U,0U,0,0L,0L,0,0,0D,6,0D,0L,0,0D,0,0,0U,0D,0,0R,0D,0R,0,0U,0,0U,0L,7,0,0L,0,0,0,0,0D,0D,0,6,0,0,0UD,5D,2,0,0,9,0,3,0L,0L,0,0,0L,0L,0,0R,0,0,0R,0,0,0L,0,0,0,0,0,0,0,0,0L,0L,0L"
      .split(",")
  (size, g.map(s => (s.head.toString.toInt, s.tail)))
