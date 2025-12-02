package adventofcode2022

import scala.util.{Failure, Success, Try}

trait Day {
  def fileName: String

  def puzzle1(input: List[String]): Any

  def puzzle2(input: List[String]): Any

  def main(args: Array[String]): Unit = {
    println("Puzzle 1 [TEST]")
    tryPuzzle(puzzle1(testInput(testInputStr1)))
    println("")
    println("Puzzle 1 [REAL DATA]")
    tryPuzzle(puzzle1(input))
    println("")
    println("Puzzle 2 [TEST]")
    tryPuzzle(puzzle2(testInput(testInputStr2)))
    println("")
    println("Puzzle 2 [REAL DATA]")
    tryPuzzle(puzzle2(input))
  }

  protected def testInput(input: String): List[String] =
    input.split("\n").toList

  def input: List[String] = readFile(fileName)

  def readFile(name: String): List[String] = {
    val file = scala.io.Source.fromFile(
      s"/Users/c.fossier/workspace/aoc/adventofcode2022/src/main/resources/$name.txt"
    )
    try file.getLines().toList
    finally file.close()
  }

  def tryPuzzle(output: Any): Unit =
    Try(println(output)) match {
      case Failure(exception) => println(exception.getMessage)
      case Success(value)     => value
    }

  protected def testInputStr1: String
  protected def testInputStr2: String = testInputStr1
}
