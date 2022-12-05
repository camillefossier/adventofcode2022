package adventofcode2022

import scala.collection.mutable.Stack
import scala.util.matching.Regex

object Day5 extends Day {
  override protected def testInputStr: String = """    [D]
                                                  |[N] [C]
                                                  |[Z] [M] [P]
                                                  | 1   2   3
                                                  |
                                                  |move 1 from 2 to 1
                                                  |move 3 from 1 to 3
                                                  |move 2 from 2 to 1
                                                  |move 1 from 1 to 2""".stripMargin

  case class Instruction(quantity: Int, src: Int, dst: Int) {
    def to(crates: List[Stack[Char]],
           preserveOrder: Boolean = false): List[Stack[Char]] = {
      val removed = 0
        .until(quantity)
        .map(_ => {
          crates(src - 1).removeLast()
        })
      val toAdd = if (preserveOrder) removed.reverse else removed
      toAdd.map(crates(dst - 1).addOne)
      crates
    }
  }

  val nb = "[0-9]+"
  val instructionRegexp: Regex =
    s"move ($nb) from ($nb) to ($nb)".r("q", "src", "dst")
  def parseInstructions(input: List[String]): List[Instruction] =
    input.flatMap(
      line =>
        instructionRegexp
          .findFirstMatchIn(line)
          .map(
            m =>
              Instruction(
                m.group("q").toInt,
                m.group("src").toInt,
                m.group("dst").toInt
            )
        )
    )

  def parseCrates(input: List[String]): List[Stack[Char]] = {
    val reversedInput = input.reverse
    val nbStacks = reversedInput.headOption
      .flatMap(_.split(" ").filter(_.nonEmpty).map(_.toInt).lastOption)
      .getOrElse(0)
    val stacks = List.fill(nbStacks)(Stack[Char]())
    reversedInput
      .safeTail(1)
      .foreach(
        line =>
          line
            .splitEvery(4)
            .zipWithIndex
            .foreach({
              case (crate, i) =>
                Option(crate.charAt(1)) match {
                  case Some(char) if char != ' ' =>
                    stacks.lift(i).map(_.addOne(char))
                  case _ => ()
                }
            })
      )
    stacks
  }

  def parse(input: List[String]): (List[Stack[Char]], List[Instruction]) =
    input.splitAt(input.indexWhere(_.isEmpty)) match {
      case (crates, instructions) =>
        (parseCrates(crates), parseInstructions(instructions))
    }

  def applyInstruction(crates: List[Stack[Char]],
                       instructions: List[Instruction],
                       preserveOrder: Boolean = false): List[Stack[Char]] =
    instructions.foldLeft(crates)(
      (crates, instruction) => instruction.to(crates, preserveOrder)
    )

  override def fileName: String = "day5"

  override def puzzle1(input: List[String]): Any =
    parse(input) match {
      case (crates, instructions) =>
        applyInstruction(crates, instructions)
          .flatMap(_.lastOption)
          .mkString("")
    }

  override def puzzle2(input: List[String]): Any =
    parse(input) match {
      case (crates, instructions) =>
        applyInstruction(crates, instructions, preserveOrder = true)
          .flatMap(_.lastOption)
          .mkString("")
    }
}