package adventofcode2022.aoc2022

import adventofcode2022.Day
import adventofcode2022.aoc2022.Day18.Drops.{findAround, parseLines}

object Day18 extends Day {
  case class Drops(cubes: Set[Point3]) {

    def solve = {
      foldWhileTrue((cubes, 0))({
        case (left, connections) if left.isEmpty => ((left, connections), false)
        case (left, connections) =>
          val head = left.head
          val neighbords = findAround(left, head)
          ((left - head, connections + neighbords.size), true)
      }) match {
        case (_, c) => cubes.size * 6 - (c * 2)
      }
    }
  }
  object Drops {
    def findAround(cubes: Set[Point3], cube: Point3): Set[Point3] =
      for {
        u <- Set(-1, 1)
        delta <- Set(Point3(u, 0, 0), Point3(0, u, 0), Point3(0, 0, u))
        neighbor = cube + delta
        if (cubes.contains(neighbor))
      } yield neighbor

    def parse(line: String): Point3 =
      line.split(",").toList match {
        case x :: y :: z :: Nil => Point3(x.toInt, y.toInt, z.toInt)
      }

    def parseLines(lines: List[String]): Drops =
      Drops(lines.map(parse).toSet)
  }
  override def fileName: String = "2022/day18"

  override def puzzle1(input: List[String]): Any = parseLines(input).solve

  override def puzzle2(input: List[String]): Any = ???

  override protected def testInputStr1: String = """2,2,2
                                                   |1,2,2
                                                   |3,2,2
                                                   |2,1,2
                                                   |2,3,2
                                                   |2,2,1
                                                   |2,2,3
                                                   |2,2,4
                                                   |2,2,6
                                                   |1,2,5
                                                   |3,2,5
                                                   |2,1,5
                                                   |2,3,5""".stripMargin
}
