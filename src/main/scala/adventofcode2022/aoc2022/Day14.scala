package adventofcode2022.aoc2022

import adventofcode2022.Day
import adventofcode2022.Utils._

object Day14 extends Day {
  type CaveMap = List[List[Char]]

  override def fileName: String = "2022/day14"

  override def puzzle1(input: List[String]): Any =
    puzzle(input, withSubFloor = false)

  def puzzle(input: List[String], withSubFloor: Boolean): Any = {
    val cave = parse(input, withSubFloor)
    val untilFull = withSubFloor
    val res = foldWhileTrue(cave)(c => {
      val evolved = c.evolve(untilFull = untilFull)
      (evolved, evolved != c)
    })
    println(res)
    res.sandCount
  }

  def parse(input: List[String], withSubFloor: Boolean): Cave = {
    val subFloorOffset = 2
    val sandPoint = Point2(500, 0)
    val linesInit = input.map(parseLine)
    val points = linesInit.flatten :+ sandPoint

    val topLeft = Point2(points.map(_.x).min, points.map(_.y).min)
    val bottomRight = Point2(points.map(_.x).max, points.map(_.y).max)
    val yPadding = 1 + subFloorOffset
    val xPadding = bottomRight.y - topLeft.y

    val minX = points.map(_.x).min - xPadding
    val maxX = points.map(_.x).max + xPadding
    val minY = points.map(_.y).min - yPadding
    val maxY = points.map(_.y).max + yPadding

    val lines =
      if (withSubFloor)
        linesInit :+ List(
          Point2(minX, maxY - yPadding + subFloorOffset),
          Point2(maxX, maxY - yPadding + subFloorOffset)
        )
      else linesInit

    val emptyMap =
      0.to(maxY - minY).toList.map(y => 0.to(maxX - minX).toList.map(x => ' '))

    val toFill = for {
      line <- lines
      (a, b) <- line.sliding(2).map({ case a :: b :: Nil => (a, b) })
      y <- if (a.y <= b.y) a.y.to(b.y) else b.y.to(a.y)
      x <- if (a.x <= b.x) a.x.to(b.x) else b.x.to(a.x)
    } yield (x - minX, y - minY)

    val map = toFill.foldLeft(emptyMap)({
      case (map, (x, y)) => map.updated(y, map(y).updated(x, '#'))
    })
    Cave(map, Point2(minX, minY), Point2(maxY, maxY), sandPoint)
  }

  def parseLine(line: String): List[Point2] =
    line
      .split(" -> ")
      .toList
      .map(_.split(",").toList match {
        case x :: y :: Nil => Point2(x.toInt, y.toInt)
      })

  override def puzzle2(input: List[String]): Any =
    puzzle(input, withSubFloor = true)

  override protected def testInputStr1: String =
    """498,4 -> 498,6 -> 496,6
      |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin

  case class Cave(map: CaveMap,
                  topLeft: Point2,
                  bottomRight: Point2,
                  sandPoint: Point2) {
    override def toString: String = map.map(_.mkString("")).mkString("\n")

    def sandCount: Int =
      map.flatten.count(_ == 'O')

    def x(p: Point2): Int =
      p.x - topLeft.x

    def y(p: Point2): Int =
      p.y - topLeft.y

    def get(p: Point2): Char =
      map(y(p))(x(p))

    def evolve(s: Point2 = sandPoint,
               untilFull: Boolean,
               checkFull: Boolean = true): Cave = {
      if (!untilFull && y(s) == bottomRight.y) this
      else if (untilFull && checkFull && get(s) == 'O') this
      else if (get(s + (y = 1)) == ' ') evolve(s + (y = 1), untilFull, false)
      else if (get(s + (x = -1, y = 1)) == ' ')
        evolve(s + (x = -1, y = 1), untilFull, false)
      else if (get(s + (x = 1, y = 1)) == ' ')
        evolve(s + (x = 1, y = 1), untilFull, false)
      else this.copy(map = map.updated(y(s), map(y(s)).updated(x(s), 'O')))
    }
  }
}
