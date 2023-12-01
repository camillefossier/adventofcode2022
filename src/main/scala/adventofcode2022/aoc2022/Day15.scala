package adventofcode2022.aoc2022

import adventofcode2022.Day
import adventofcode2022.aoc2022.Day15.SensorsMap.manhattan

import scala.math.abs

object Day15 extends Day {
  case class SensorsMap(sensors: List[Point2] = List.empty,
                        beacons: List[Point2] = List.empty,
                        closest: Map[Point2, Point2] = Map.empty) {
    def add(sensor: Point2, beacon: Point2): SensorsMap =
      copy(
        sensors = sensors :+ sensor,
        beacons = beacons :+ beacon,
        closest = closest + (sensor -> beacon)
      )

    def topLeft: Point2 =
      Point2(
        sensors.map(_.x).min + beacons.map(_.x).min,
        sensors.map(_.y).min + beacons.map(_.y).min
      )

    def bottomRight: Point2 =
      Point2(
        sensors.map(_.x).max + beacons.map(_.x).max,
        sensors.map(_.y).max + beacons.map(_.y).max
      )

    def x(p: Point2): Int =
      p.x - topLeft.x

    def y(p: Point2): Int =
      p.y - topLeft.y

    def emptyMap: List[List[Int]] =
      0.to(y(bottomRight))
        .toList
        .map(_ => 0.to(x(bottomRight)).toList.map(_ => 0))

    def filledMap: List[List[Int]] =
      closest.foldLeft(emptyMap)({
        case (map, (sensor, beacon)) => fillMap(map, sensor, beacon)
      })

    def fillMap(map: List[List[Int]],
                sensor: Point2,
                beacon: Point2): List[List[Int]] = {
      val maxDist = manhattan(sensor, beacon)
      val toFill = for {
        dx <- 0.to(maxDist)
        dy <- 0.to(maxDist - dx)
        multX <- List(-1, 1)
        multY <- List(-1, 1)
      } yield Point2(sensor.x + (dx * multX), sensor.y + (dy * multY))
      toFill.foldLeft(map)(
        (acc, p) =>
          (for {
            vy <- map.lift(y(p))
            _ <- vy.lift(x(p))
          } yield acc.updated(y(p), acc(y(p)).updated(x(p), 1))).getOrElse(acc)
      )
    }
  }

  object SensorsMap {
    def manhattan(p1: Point2, p2: Point2): Int =
      abs(p1.x - p2.x) + abs(p1.y - p2.y)
  }
  def parseLine(line: String): Option[(Point2, Point2)] = {
    val nb = "[0-9]+"
    val regex =
      s"Sensor at x=($nb), y=($nb): closest beacon is at x=($nb), y=($nb)".r(
        "sx",
        "sy",
        "bx",
        "by"
      )
    regex
      .findFirstMatchIn(line)
      .map(
        m =>
          (
            Point2(m.group("sx").toInt, m.group("sy").toInt),
            Point2(m.group("sx").toInt, m.group("sy").toInt)
        )
      )
  }

  def parse(lines: List[String]): SensorsMap =
    lines
      .flatMap(parseLine)
      .foldLeft(SensorsMap())({
        case (acc, (sensor, beacon)) => acc.add(sensor, beacon)
      })

  override def fileName: String = "2022/day15"

  override def puzzle1(input: List[String]): Any =
    parse(input).filledMap.map(_.mkString("")).mkString("\n")

  override def puzzle2(input: List[String]): Any = ???

  override protected def testInputStr1: String =
    """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
                                                   |Sensor at x=9, y=16: closest beacon is at x=10, y=16
                                                   |Sensor at x=13, y=2: closest beacon is at x=15, y=3
                                                   |Sensor at x=12, y=14: closest beacon is at x=10, y=16
                                                   |Sensor at x=10, y=20: closest beacon is at x=10, y=16
                                                   |Sensor at x=14, y=17: closest beacon is at x=10, y=16
                                                   |Sensor at x=8, y=7: closest beacon is at x=2, y=10
                                                   |Sensor at x=2, y=0: closest beacon is at x=2, y=10
                                                   |Sensor at x=0, y=11: closest beacon is at x=2, y=10
                                                   |Sensor at x=20, y=14: closest beacon is at x=25, y=17
                                                   |Sensor at x=17, y=20: closest beacon is at x=21, y=22
                                                   |Sensor at x=16, y=7: closest beacon is at x=15, y=3
                                                   |Sensor at x=14, y=3: closest beacon is at x=15, y=3
                                                   |Sensor at x=20, y=1: closest beacon is at x=15, y=3""".stripMargin
}
