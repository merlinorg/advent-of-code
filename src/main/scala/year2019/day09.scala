package org.merlin.aoc
package year2019.day09

import lib.{*, given}

@main
def part1(): Unit =
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(actual))

val actual: String = load("actual.txt")

def part1(input: String): Long =
  Computer(input.parse, Vector(1L)).run.last().output.last

def part2(input: String): Long =
  Computer(input.parse, Vector(2L)).run.last().output.last

case class Computer(pc: Long, memory: Map[Long, Long], input: Vector[Long], output: Vector[Long], relative: Long):
  def runIO: (Long, Computer) =
    run.findMap: c =>
      c.output.headOption.strengthR(c.copy(output = Vector.empty))

  def run: Iterator[Computer] = Iterator.iteropt(this)(_.step)

  def done: Boolean = memory(pc) == 99

  def a(i: Int): Long = (memory(pc) / (10 ** (i + 1))) % 10 match
    case 1 => pc + i
    case 2 => relative + memory(pc + i)
    case _ => memory(pc + i)

  def r(i: Int): Long = memory(a(i))

  def step: Option[Computer] = PartialFunction.condOpt(memory(pc) % 100):
    case 1 => copy(pc = pc + 4, memory = memory.updated(a(3), r(1) + r(2)))
    case 2 => copy(pc = pc + 4, memory = memory.updated(a(3), r(1) * r(2)))
    case 3 => copy(pc = pc + 2, memory = memory + (a(1) -> input.head), input = input.tail)
    case 4 => copy(pc = pc + 2, output = output :+ r(1))
    case 5 => copy(pc = if r(1) != 0 then r(2) else pc + 3)
    case 6 => copy(pc = if r(1) == 0 then r(2) else pc + 3)
    case 7 => copy(pc = pc + 4, memory = memory.updated(a(3), if r(1) < r(2) then 1 else 0))
    case 8 => copy(pc = pc + 4, memory = memory.updated(a(3), if r(1) == r(2) then 1 else 0))
    case 9 => copy(pc = pc + 2, relative = relative + r(1))

object Computer:
  def apply(memory: Map[Long, Long], input: Vector[Long]): Computer =
    new Computer(pc = 0L, memory, input, output = Vector.empty, relative = 0L)

extension (self: String)
  def parse: Map[Long, Long] = self.longs.zipWithIndex.mapToMap(_.swap.lmap(_.toLong)).withDefaultValue(0L)
