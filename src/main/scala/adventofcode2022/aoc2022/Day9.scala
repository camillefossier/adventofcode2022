package adventofcode2022.aoc2022

import adventofcode2022.Day

import scala.math.{abs, max}

object Day9 extends Day {

  override def fileName: String = "2022/day9"

  override def puzzle1(input: List[String]): Any =
    puzzle(input, 2)

  override def puzzle2(input: List[String]): Any =
    puzzle(input, 10)

  private def puzzle(input: List[String], nbKnots: Int) = {
    val lastKnot = input
      .foldLeft(Rope(knots = 1.to(nbKnots).toList.map(_ => Knot())).init())(
        (rope, line) => rope.applyMovement(line)
      )
      .knots
      .last
    println(lastKnot.renderPositions)
    lastKnot.visitedPositions.toSet.size
  }

  override protected def testInputStr1: String = """R 4
                                                   |U 4
                                                   |L 3
                                                   |D 1
                                                   |R 4
                                                   |D 1
                                                   |L 5
                                                   |R 2""".stripMargin

  override protected def testInputStr2: String = """R 5
                                                  |U 8
                                                  |L 8
                                                  |D 3
                                                  |R 17
                                                  |D 10
                                                  |L 25
                                                  |U 20""".stripMargin

  case class Knot(x: Int = 0,
                  y: Int = 0,
                  visitedPositions: List[(Int, Int)] = List.empty) {

    def renderPositions: String = {
      val (minX, maxX) =
        (visitedPositions.map(_._1).min, visitedPositions.map(_._1).max)
      val (minY, maxY) =
        (visitedPositions.map(_._2).min, visitedPositions.map(_._2).max)
      val offsetPositions = visitedPositions.map({
        case (x, y) => (x - minX, y - minY)
      })
      val X = maxX - minX
      val Y = maxY - minY
      val grid = 0
        .to(Y)
        .toList
        .map(_ => 0.to(X).toList.map(_ => "."))
      val path = offsetPositions.toSet.foldLeft(grid)({
        case (g, (x, y)) =>
          g.updated(y, g(y).updated(x, "#"))
      })
      path.map(_.mkString("")).reverse.mkString("\n")
    }

    def move(dx: Int, dy: Int): Knot =
      this.copy(x = x + dx, y = y + dy)

    def moveToward(knot: Knot, maxLength: Int): Knot =
      if (Knot.length(this, knot) > maxLength)
        this.copy(x = x + ux(knot), y = y + uy(knot)).addNewPosition()
      else this

    def ux(to: Knot): Int = if (x == to.x) 0 else (to.x - x) / abs(to.x - x)

    def uy(to: Knot): Int = if (y == to.y) 0 else (to.y - y) / abs(to.y - y)

    def addNewPosition(): Knot =
      this.copy(visitedPositions = visitedPositions :+ (x, y))
  }

  case class Rope(knots: List[Knot] = List(Knot(), Knot()),
                  maxLength: Int = 1) {

    def init(): Rope =
      this.copy(knots = knots.map(_.addNewPosition()))

    def move(dx: Int, dy: Int): Rope = {
      knots.indices
        .sliding(2)
        .foldLeft(this.copy(knots = knots.updated(0, knots.head.move(dx, dy))))(
          (rope, i) =>
            rope.copy(
              knots = rope.knots.updated(
                i(1),
                rope.knots(i(1)).moveToward(rope.knots(i(0)), maxLength)
              )
          )
        )
    }

    def moveRight(): Rope = move(1, 0)
    def moveLeft(): Rope = move(-1, 0)
    def moveUp(): Rope = move(0, 1)
    def moveDown(): Rope = move(0, -1)

    def applyMovement(line: String): Rope =
      line.split(" ").toList match {
        case "R" :: nb :: Nil =>
          1.to(nb.toInt).foldLeft(this)((rope, _) => rope.moveRight())
        case "L" :: nb :: Nil =>
          1.to(nb.toInt).foldLeft(this)((rope, _) => rope.moveLeft())
        case "U" :: nb :: Nil =>
          1.to(nb.toInt).foldLeft(this)((rope, _) => rope.moveUp())
        case "D" :: nb :: Nil =>
          1.to(nb.toInt).foldLeft(this)((rope, _) => rope.moveDown())
      }
  }

  object Knot {
    def length(a: Knot, b: Knot): Int =
      max(horizontalLength(a, b), verticalLength(a, b))

    def horizontalLength(a: Knot, b: Knot): Int = abs(a.x - b.x)

    def verticalLength(a: Knot, b: Knot): Int = abs(a.y - b.y)
  }
}
