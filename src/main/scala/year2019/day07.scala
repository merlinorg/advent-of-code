package org.merlin.aoc
package year2019.day07

import lib.{*, given}

@main
def part1(): Unit =
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(actual))

val actual: String = load("actual.txt")

def part1(input: String): Long =
  val program = input.parse
  (0 to 4).permutations.maxMap: phases =>
    phases.foldLeft(0): (input, phase) =>
      Computer(program, Vector(phase, input)).run.last().output.last

def part2(input: String): Long =
  val program = input.parse
  (5 to 9).permutations.maxMap: phases =>
    Iterator
      .iterate((0, phases.toVector.map(phase => Computer(program, Vector(phase))))):
        case (input, computers) =>
          computers.mapAcc(input): (input, computer) =>
            computer.copy(input = computer.input :+ input).runIO
      .findMap: (input, computers) =>
        Option.when(computers.head.done)(input)

case class Computer(pc: Int, memory: Map[Int, Int], input: Vector[Int], output: Vector[Int]):
  def runIO: (Int, Computer) =
    run.findMap: c =>
      c.output.headOption.strengthR(c.copy(output = Vector.empty))

  def run: Iterator[Computer] = Iterator.iteropt(this)(_.step)

  def done: Boolean = memory(pc) == 99

  def r(i: Int): Int = (memory(pc) / (10 ** (i + 1))) % 10 match
    case 1 => memory(pc + i)
    case _ => memory(memory(pc + i))

  def step: Option[Computer] = PartialFunction.condOpt(memory(pc) % 100):
    case 1 => copy(pc = pc + 4, memory = memory.updated(memory(pc + 3), r(1) + r(2)))
    case 2 => copy(pc = pc + 4, memory = memory.updated(memory(pc + 3), r(1) * r(2)))
    case 3 => copy(pc = pc + 2, memory = memory + (memory(pc + 1) -> input.head), input = input.tail)
    case 4 => copy(pc = pc + 2, output = output :+ r(1))
    case 5 => copy(pc = if r(1) != 0 then r(2) else pc + 3)
    case 6 => copy(pc = if r(1) == 0 then r(2) else pc + 3)
    case 7 => copy(pc = pc + 4, memory = memory.updated(memory(pc + 3), if r(1) < r(2) then 1 else 0))
    case 8 => copy(pc = pc + 4, memory = memory.updated(memory(pc + 3), if r(1) == r(2) then 1 else 0))

object Computer:
  def apply(memory: Map[Int, Int], input: Vector[Int]): Computer = new Computer(0, memory, input, Vector.empty)

extension (self: String) def parse: Map[Int, Int] = self.integers.zipWithIndex.mapToMap(_.swap).withDefaultValue(0)
