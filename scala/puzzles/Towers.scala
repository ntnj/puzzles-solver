package puzzles

import scala.util.chaining.scalaUtilChainingOps

@main def Towers(Size: Int = 6): Unit =
  // https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/towers.html#9:3/3///4/3/1//3///3/3///4/7/3/3//2/5/5/2//2/2///3////6//2,c2g3b1p3f6a8b1f4a5c5e3d6_2l
  val (d, g) = Size match
    case 6 => ("4/2/4///////3////////1///2/4//4", "e3i3c2")
    case 7 => ("/4//4//2////2/2/2/3///3/3/4/3////3//3/2//4", "a2q7b3s4e2")
    case 8 => ("3//5//4/3//4/5/2/3///2/5///3/////4/6///3/5/3//3/", "q1b3f3e3c4g1")
    case 9 => ("3/3///4/3/1//3///3/3///4/7/3/3//2/5/5/2//2/2///3////6//2", "c2g3b1p3f6a8b1f4a5c5e3d6_2")

  val dir = TowersGrid.parseDir(Size, d)
  val initial = TowersGrid(Size, dir, IArray.fill(Size, Size)((1 << Size) - 1))
  val solved = TowersGrid
    .parse(Size, g)
    .foldLeft(Option(initial)) { case (g, (i, j, v)) =>
      g.flatMap(_.assign(i, j, v))
    }
    .flatMap(_.solve())
  assert(solved.isDefined)
  println(s"${TowersGrid.steps} steps")
  // solved.get.display()
  solved.get.prettydisplay()

case class TowersDir(up: IArray[Int], down: IArray[Int], left: IArray[Int], right: IArray[Int])

class TowersGrid(val Size: Int, var dir: TowersDir, val grid: IArray[IArray[Int]]):
  inline private def cell(i: Int, j: Int) = grid(i - 1)(j - 1)
  private def solved(s: Seq[Int]): Boolean = s.forall(Integer.bitCount(_) == 1)
  private def col(j: Int): Seq[Int] = (1 to Size).map(cell(_, j))
  private def cross[T](a: Seq[T], b: Seq[T]): Seq[(T, T)] = for x <- a; y <- b yield (x, y)
  private def peers(i: Int, j: Int): Seq[(Int, Int)] = blocks(i, j).flatten
  private def blocks(i: Int, j: Int): Seq[Seq[(Int, Int)]] =
    Seq(
      (1 to Size).filter(_ != i).map((_, j)),
      (1 to Size).filter(_ != j).map((i, _))
    )

  private def visible(v: Seq[Int]): Int = if v.isEmpty then 0 else 1 + visible(v.tail.dropWhile(_ < v.head))
  private def maybeCorrect(h: Int, v: Seq[Int]): Boolean = h match
    case 0 => true
    // case 1 if Integer.bitCount(v(0)) == 1 => v.tail.forall(_ < v(0))
    case 1 => v.tail.forall(Integer.lowestOneBit(_) < Integer.highestOneBit(v(0)))
    case _ => true
    // case h => maybeCorrect(h-1, v.takeWhile(Integer.lowestOneBit(_) <= Integer.highestOneBit(v(0)))).tap: check =>
    // maybeCorrect(h-1,v.dropWhile(Integer.highestOneBit(_) <= Integer.lowestOneBit(v(0)))).tap: check =>
    // if !check then println(f"$h ${v.map(Integer.toBinaryString)}%6s")

  def assign(i: Int, j: Int, v: Int): Option[TowersGrid] =
    if ((1 << (v - 1)) & cell(i, j)) == 0 then None
    else if Integer.bitCount(cell(i, j)) == 1 then Some(this)
    else
      Some(TowersGrid(Size, dir, grid.updated(i - 1, grid(i - 1).updated(j - 1, 1 << (v - 1))))).flatMap: sg =>
        peers(i, j)
          .foldLeft(Option(sg)) { case (ng, (i, j)) => ng.flatMap(_.remove(i, j, v)) }
          .filter: sg =>
            val d = Size - v
            dir.left(i - 1) - d <= j && dir.right(i - 1) - d <= Size + 1 - j && dir.up(j - 1) - d <= i && dir.down(j - 1) - d <= Size + 1 - i
          .filter: sg =>
            !solved(sg.grid(i - 1)) || ((dir.left(i - 1) == 0 || visible(sg.grid(i - 1)) == dir.left(i - 1))
              && (dir.right(i - 1) == 0 || visible(sg.grid(i - 1).reverse) == dir.right(i - 1)))
          .filter: sg =>
            !solved(sg.col(j)) || ((dir.up(j - 1) == 0 || visible(sg.col(j)) == dir.up(j - 1))
              && (dir.down(j - 1) == 0 || visible(sg.col(j).reverse) == dir.down(j - 1)))
          .filter: sg =>
            val check = maybeCorrect(dir.left(i - 1), sg.grid(i - 1)) && maybeCorrect(dir.right(i - 1), sg.grid(i - 1).reverse)
              && maybeCorrect(dir.up(j - 1), sg.col(j)) && maybeCorrect(dir.down(j - 1), sg.col(j).reverse)
            if !check then sg.display()
            check

  def remove(i: Int, j: Int, v: Int): Option[TowersGrid] =
    if ((1 << (v - 1)) & cell(i, j)) == 0 then Some(this)
    else
      Some(TowersGrid(Size, dir, grid.updated(i - 1, grid(i - 1).updated(j - 1, cell(i, j) & ~(1 << (v - 1)))))).flatMap: sg =>
        if sg.cell(i, j) == 0 then None
        else if Integer.bitCount(sg.cell(i, j)) == 1 then assign(i, j, Integer.numberOfTrailingZeros(sg.cell(i, j)) + 1)
        else
          blocks(i, j).foldLeft(Option(sg)): (sg, bls) =>
            sg.flatMap: sg =>
              bls.filter((i, j) => (sg.cell(i, j) & (1 << (v - 1))) != 0) match
                case Nil         => None
                case Seq((i, j)) => sg.assign(i, j, v)
                case _           => Some(sg)

  def solve(indent: String = ""): Option[TowersGrid] =
    if grid.forall(_.forall(Integer.bitCount(_) == 1)) then Some(this)
    else
      cross(1 to Size, 1 to Size)
        .filter { case (i, j) => Integer.bitCount(cell(i, j)) > 1 }
        .minByOption { case (i, j) => Integer.bitCount(cell(i, j)) }
        .flatMap: (i, j) =>
          (1 to Size).foldLeft[Option[TowersGrid]](None): (sg, v) =>
            sg.orElse:
              if ((1 << (v - 1)) & cell(i, j)) == 0 then None
              else
                TowersGrid.steps += 1
                println(s"${indent}guess($i, $j) = $v")
                assign(i, j, v).flatMap(_.solve(indent + " "))

  def display(): Unit = println(grid.map(_.map(v => f"${Integer.toBinaryString(v)}%9s".replace(' ', '0')).mkString(" ")).mkString("\n"))

  def prettydisplay(): Unit =
    println(grid.map(_.map(v => if Integer.bitCount(v) == 1 then Integer.numberOfTrailingZeros(v) + 1 else ' ').mkString(" ")).mkString("\n"))

object TowersGrid:
  var steps = 0
  // https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/towers.html
  def parse(size: Int, in: String): Seq[(Int, Int, Int)] =
    Seq
      .unfold((0, in)): (pos, v) =>
        if v.isEmpty() then None
        else
          val newPos = pos + math.max(v(0) - 'a' + 1, 0) + 1
          Some((newPos, v(1) - '0'), (newPos, v.drop(2)))
      .map: (i, v) =>
        ((i - 1) / size + 1, (i - 1) % size + 1, v)

  def parseDir(size: Int, in: String): TowersDir =
    val ss = IArray.unsafeFromArray(in.split("/")).map(s => if s == "" then 0 else s.toInt)
    assert(ss.length == size * 4)
    val dirs = ss.grouped(size).toSeq
    TowersDir(dirs(0), dirs(1), dirs(2), dirs(3))
