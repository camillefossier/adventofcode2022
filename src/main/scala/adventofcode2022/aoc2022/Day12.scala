package adventofcode2022.aoc2022

import adventofcode2022.Day

import scala.math.min

object Day12 extends Day {
  override def fileName: String = "2022/day12"

  case class HeightMap(map: List[List[Point3]],
                       start: Point3,
                       end: Point3,
                       visited: Set[Point3] = Set.empty) {
    def solve2() = {
      var current: Option[Point3] = None
      var possibleDirections: List[Set[Point3]] = List(Set(start))
      var paths: List[List[Point3]] = List()
      var currentPath: List[Point3] = List()
      var stop = false
      while (!stop) {

        if (current.contains(end)) {
          paths = paths :+ currentPath
          currentPath = currentPath.dropRight(1)
          current = currentPath.lastOption
          possibleDirections = possibleDirections.dropRight(1)
        }

        possibleDirections.reverse match {
          case Nil => stop = true
          case lastLayer :: _ => {

            lastLayer.toList match {
              case Nil =>
                possibleDirections = possibleDirections.dropRight(1)
                currentPath = currentPath.dropRight(1)
                current = currentPath.lastOption
              case c :: rest =>
                current = Some(c)
                currentPath = currentPath :+ c
                val newLayer = (for {
                  (x, y) <- List((-1, 0), (1, 0), (0, -1), (0, 1))
                  adjacentPoint = map.lift(c.y + y).flatMap(_.lift(c.x + x))
                  if (adjacentPoint
                    .fold(false)(
                      p =>
                        p.z - c.z <= 1 && !currentPath.contains(p) && {
                          paths.isEmpty || paths
                            .map(path => path.indexWhere(e => e == p))
                            .min > currentPath.length
                      }
                    ))
                } yield adjacentPoint).flatten.toSet
                possibleDirections = possibleDirections.dropRight(1) :+ rest.toSet :+ newLayer
            }

          }
        }
      }
      paths
    }

    def adjacent(p: Point3, goDown: Boolean = false): List[Point3] =
      (for {
        (x, y) <- List((-1, 0), (1, 0), (0, -1), (0, 1))
        adjacentPoint = map.lift(p.y + y).flatMap(_.lift(p.x + x))
        if (adjacentPoint.fold(false)(n => {
          if (goDown) p.z - n.z <= 1 else n.z - p.z <= 1
        }))
      } yield adjacentPoint).flatten

    type Path = List[Point3]
    type ScoreMap = Map[Point3, Int]

    def solve4(start: Point3 = start,
               map: Map[Point3, Int] = Map(start -> 0),
               goDown: Boolean = false): Map[Point3, Int] = {
      val currScore = map(start)
      val adj =
        adjacent(start, goDown).filter(map.get(_).forall(_ > currScore + 1))
      val newMap =
        adj.foldLeft(map)((acc, curr) => acc + (curr -> (currScore + 1)))
      adj.foldLeft(newMap)((acc, curr) => solve4(curr, acc, goDown))
    }

    def solve3(curr: Point3 = start,
               currentPath: List[Point3] = List.empty,
               scores: Map[Point3, Int] = Map.empty,
               maxSize: Int): (Option[Path], ScoreMap) = {
      val curlen = currentPath.length
      if (curr == end) {
        val newScores =
          (currentPath :+ curr).reverse.zipWithIndex.foldLeft(scores)({
            case (acc, (p, score)) =>
              acc + (p -> acc.get(p).fold(score)(min(_, score)))
          })
        (Some(currentPath :+ curr), newScores)
      } else {
        val nScores = scores + (curr -> scores
          .get(curr)
          .fold(maxSize)(min(_, maxSize)))
        val adjacents = adjacent(curr)
          .filterNot(currentPath.contains)
          .map(e => e -> scores.get(e))
        val newScores =
          /*if (adjacents.map(_._2).toSet.size == 1)
            adjacents.map(_._1).foldLeft(nScores)((acc, p) => acc - p)
          else*/
          nScores
        adjacents.sortBy(_._2).map(_._1) match {
          case Nil => (None, newScores)
          /*(None, (currentPath :+ curr).foldLeft(scores)({
              case (acc, p) =>
                acc + (p -> acc.get(p).fold(maxSize)(min(_, maxSize)))
            }))*/
          case next :: Nil =>
            solve3(next, currentPath :+ curr, newScores, maxSize)
          case next :: _ =>
            solve3(next, currentPath :+ curr, newScores, maxSize)
        }
      }
    }

    def solve33() = {
      var stop = false
      var best: Option[Path] = Some(List())
      var scores = Map.empty[Point3, Int]
      while (!stop) {
        val (found, newScores) =
          solve3(start, maxSize = map.head.size * map.size, scores = scores)
        if (found.exists(f => best.exists(b => b == f))) stop = true
        best = found
        scores = newScores
      }
      best
    }

    /*if (currentPath.isEmpty) stop = true
        current match {
          case Some(e) if e == end =>
            paths = paths :+ currentPath
          case Some(c) =>
            visited2 = visited2 + c

            // Find possible directions
            val newLayer = (for {
              (x, y) <- List((-1, 0), (1, 0), (0, -1), (0, 1))
              adjacentPoint = map.lift(c.x + x).flatMap(_.lift(c.y + y))
              if (adjacentPoint
                .fold(false)(p => p.z - c.z <= 1 && !visited2.contains(p)))
            } yield adjacentPoint).flatten

            newLayer match {
              case head :: rest =>
                current = head
                possibleDirections
            }
            // Pick next
            val candidate = possibleDirections.lastOption.flatMap(
              _.find(!visited2.contains(_))
            )

            candidate match {
              case Some(next) =>
                current = Some(next)
                currentPath = currentPath ++ current
              case None =>
                currentPath = currentPath.dropRight(1)
                current = currentPath.lastOption
            }*/

    /*visited2 = visited2 + current
            if (current == end) {
              paths = paths :+ currentPath
            }

            if (possibleDirections.last.nonEmpty) {
              current = possibleDirections.last.head
              currentPath = currentPath :+ current
              val newLast = possibleDirections.last - current
              possibleDirections = possibleDirections.dropRight(1)
              if (newLast.nonEmpty)
                possibleDirections = possibleDirections :+ newLast
            } else {
              currentPath = currentPath.dropRight(1)
              if (currentPath.nonEmpty) current = currentPath.last
              possibleDirections = possibleDirections.dropRight(1)
            }*/
    /*case None => stop = true
        }
      }
      paths
    }*/

    def solve(): List[Set[Point3]] = {
      if (visited.contains(end)) {
        List(visited)
      } else {
        val possibleDirections = for {
          (x, y) <- List((-1, 0), (1, 0), (0, -1), (0, 1))
          adjacentPoint = map.lift(start.y + y).flatMap(_.lift(start.x + x))
          if (adjacentPoint.fold(false)(
            p => p.z - start.z <= 1 && !visited.contains(p)
          ))
        } yield adjacentPoint

        possibleDirections.flatten.flatMap(
          p => HeightMap(map, p, end, visited + start).solve()
        )
      }
    }
  }

  def parse(input: List[String]): HeightMap = {
    val flatInput = input.flatten
    val rowSize = input.headOption.map(_.length).getOrElse(0)
    val start = flatInput.indexWhere(_ == 'S')
    val end = flatInput.indexWhere(_ == 'E')
    val hm = HeightMap(
      List.empty,
      start = Point3(start % rowSize, start / rowSize, 0),
      end = Point3(end % rowSize, end / rowSize, 25)
    )
    val coords = input.zipWithIndex.map({
      case (line, y) =>
        line.toCharArray.toList.zipWithIndex.map({
          case ('S', x) => Point3(x, y, 0)
          case ('E', x) => Point3(x, y, 25)
          case (c, x)   => Point3(x, y, c - 97)
        })
    })
    hm.copy(map = coords)
  }

  override def puzzle1(input: List[String]): Any = {
    val i = parse(input)
    i.solve4().get(i.end)
  }

  override def puzzle2(input: List[String]): Any = {
    val i = parse(input)
    val map = i.solve4(i.end, Map(i.end -> 0), true)
    map
      .filter({ case (p, _) => p.z == 0 })
      .values
      .min
  }

  override protected def testInputStr1: String = """Sabqponm
                                                   |abcryxxl
                                                   |accszExk
                                                   |acctuvwj
                                                   |abdefghi""".stripMargin
}
