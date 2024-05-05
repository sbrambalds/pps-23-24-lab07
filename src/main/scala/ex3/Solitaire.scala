package ex3

import javax.print.attribute.standard.Destination
import scala.collection
import scala.util.Random

object Solitaire:
  type Cell = (Int, Int)
  type Solution = Iterable[Cell]
  type IterableFactory = Solution => Iterable[Solution]

  private val width = 5
  private val height = 5
  private val start: Cell = (width/2, height/2)

  given IterableFactory = LazyList(_)

  def placeMarks(movesLeft: Int = width*height)(using factory: IterableFactory): Iterable[Solution] = movesLeft match
    case 0 => factory(List(start))
    case _ =>
      val positions = placeMarks(movesLeft - 1)
      for
        pos <- positions
        x <- 0 to width
        y <- 0 to height
        cell = (x, y)
        if valid(pos, cell)
      yield
        pos.++(List(cell))

  def valid(solution: Iterable[Cell], dest: Cell): Boolean =  !solution.exists(p => p == dest) & correct(solution.last, dest)

  def correct(source: Cell, dest: Cell): Boolean =
    val x = (source._1 - dest._1).abs
    val y = (source._2 - dest._2).abs
    (x == 3 & y == 0) | (x == 0 & y == 3) | (x == 2 & y == 2)

  def render(sol: Seq[Cell]): String =
    val reversed = sol
    val rows = for y <- 0 until height
        row = for x <- 0 until width
        number = reversed.indexOf((x,y)) + 1
        yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  @main def run (): Unit = placeMarks().zipWithIndex.foreach({solution => println("Solution " + solution._2); println(render(solution._1.toList))})
