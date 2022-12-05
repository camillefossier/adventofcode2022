package adventofcode2022

import scala.util.{Failure, Success, Try}

trait Day {
  def readFile(name: String): List[String] = {
    val file = scala.io.Source.fromFile(
      s"/Users/c.fossier/workspace/adventofcode2022/app/src/main/resources/$name.txt"
    )
    try file.getLines().toList
    finally file.close()
  }

  protected def testInputStr: String
  protected def testInput: List[String] = testInputStr.split("\n").toList

  def fileName: String
  def input: List[String] = readFile(fileName)

  def puzzle1(input: List[String]): Any
  def puzzle2(input: List[String]): Any

  def tryPuzzle(output: Any): Unit =
    Try(println(output)) match {
      case Failure(exception) => println(exception.getMessage)
      case Success(value)     => value
    }

  def main(args: Array[String]): Unit = {
    println("Puzzle 1 [TEST]")
    tryPuzzle(puzzle1(testInput))
    println("Puzzle 1 [REAL DATA]")
    tryPuzzle(puzzle1(input))
    println("Puzzle 2 [TEST]")
    tryPuzzle(puzzle2(testInput))
    println("Puzzle 2 [REAL DATA]")
    tryPuzzle(puzzle2(input))
  }
}
