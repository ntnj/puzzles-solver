package puzzles

@main def Sudoku: Unit =
  val initial = SudokuGrid(IArray.fill(9, 9)((1 << 9) - 1))
  // https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/solo.html#3x3:a7b4e4_9a1e5a3b2a7e3g5_9b3_9_6e7_4a2_5d8c4b6b5g6a
  val solved = SudokuGrid
    .parse("a7b4e4_9a1e5a3b2a7e3g5_9b3_9_6e7_4a2_5d8c4b6b5g6")
    .foldLeft(Option(initial)) { case (g, (i, j, v)) =>
      g.flatMap(_.assign(i, j, v))
    }
    .flatMap(_.solve())
  assert(solved.isDefined)
  // solved.get.display()
  solved.get.prettydisplay()

class SudokuGrid(val grid: IArray[IArray[Int]]):
  inline private def cell(i: Int, j: Int) = grid(i - 1)(j - 1)
  private val subgrid: Seq[Range] = (1 to 9).grouped(3).flatMap(Seq.fill(3)).toSeq
  private def cross[T](a: Seq[T], b: Seq[T]): Seq[(T, T)] = for x <- a; y <- b yield (x, y)
  private def peers(i: Int, j: Int): Seq[(Int, Int)] = blocks(i, j).flatten
  private def blocks(i: Int, j: Int): Seq[Seq[(Int, Int)]] =
    Seq(
      (1 to 9).filter(_ != i).map((_, j)),
      (1 to 9).filter(_ != j).map((i, _)),
      cross(subgrid(i - 1), subgrid(j - 1)).filter(_ != i || _ != j)
    )

  def assign(i: Int, j: Int, v: Int): Option[SudokuGrid] =
    if ((1 << (v - 1)) & cell(i, j)) == 0 then None
    else if Integer.bitCount(cell(i, j)) == 1 then Some(this)
    else
      Some(SudokuGrid(grid.updated(i - 1, grid(i - 1).updated(j - 1, 1 << (v - 1))))).flatMap: sg =>
        peers(i, j).foldLeft(Option(sg)) { case (ng, (i, j)) => ng.flatMap(_.remove(i, j, v)) }

  def remove(i: Int, j: Int, v: Int): Option[SudokuGrid] =
    if ((1 << (v - 1)) & cell(i, j)) == 0 then Some(this)
    else
      Some(SudokuGrid(grid.updated(i - 1, grid(i - 1).updated(j - 1, cell(i, j) & ~(1 << (v - 1)))))).flatMap: sg =>
        if sg.cell(i, j) == 0 then None
        else if Integer.bitCount(sg.cell(i, j)) == 1 then assign(i, j, Integer.numberOfTrailingZeros(sg.cell(i, j)) + 1)
        else
          blocks(i, j).foldLeft(Option(sg)): (sg, bls) =>
            sg.flatMap: sg =>
              bls.filter((i, j) => (sg.cell(i, j) & (1 << (v - 1))) != 0) match
                case Nil         => None
                case Seq((i, j)) => sg.assign(i, j, v)
                case _           => Some(sg)

  def solve(indent: String = ""): Option[SudokuGrid] =
    if grid.forall(_.forall(Integer.bitCount(_) == 1)) then Some(this)
    else
      // Optimize choosing the one with minimum solutions left
      cross(1 to 9, 1 to 9)
        .find { case (i, j) => Integer.bitCount(cell(i, j)) > 1 }
        .flatMap: (i, j) =>
          (1 to 9).foldLeft[Option[SudokuGrid]](None): (sg, v) =>
            sg.orElse:
              if ((1 << (v - 1)) & cell(i, j)) == 0 then None
              else
                println(s"${indent}guess($i, $j) = $v")
                assign(i, j, v).flatMap(_.solve(indent + " "))

  def display(): Unit = println(grid.map(_.map(v => f"${Integer.toBinaryString(v)}%9s".replace(' ', '0')).mkString(" ")).mkString("\n"))

  def prettydisplay(): Unit =
    println(grid.map(_.map(v => if Integer.bitCount(v) == 1 then Integer.numberOfTrailingZeros(v) + 1 else ' ').mkString(" ")).mkString("\n"))

object SudokuGrid:
  // https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/solo.html
  def parse(in: String): Seq[(Int, Int, Int)] =
    Seq
      .unfold((0, in)): (pos, v) =>
        if v.isEmpty() then None
        else
          val newPos = pos + math.max(v(0) - 'a' + 1, 0) + 1
          Some((newPos, v(1) - '0'), (newPos, v.drop(2)))
      .map: (i, v) =>
        ((i - 1) / 9 + 1, (i - 1) % 9 + 1, v)
