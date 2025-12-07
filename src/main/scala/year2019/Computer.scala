package org.merlin.aoc
package year2019

import lib.*

case class Computer(pc: Long, memory: Map[Long, Long], input: Vector[Long], output: Vector[Long], relative: Long):

  def run: Iterator[Computer] = Iterator.iteropt(this)(_.step)

  def runIO: Option[(Long, Computer)] =
    run.findMapOpt: c =>
      c.output.headOption.strengthR(c.copy(output = Vector.empty))

  def unfoldIO: Iterator[Long] = Iterator.unfold(this)(_.runIO)

  def done: Boolean = memory(pc) == 99

  def a(i: Int): Long = (memory(pc) / (10 ** (i + 1))) % 10 match
    case 1 => pc + i
    case 2 => relative + memory(pc + i)
    case _ => memory(pc + i)

  def r(i: Int): Long = memory(a(i))

  def step: Option[Computer] = PartialFunction.condOpt(memory(pc) % 100):
    case 1                  => copy(pc = pc + 4, memory = memory.updated(a(3), r(1) + r(2)))
    case 2                  => copy(pc = pc + 4, memory = memory.updated(a(3), r(1) * r(2)))
    case 3 if input.isEmpty => copy(pc = pc + 2, memory = memory + (a(1) -> -1))
    case 3                  => copy(pc = pc + 2, memory = memory + (a(1) -> input.head), input = input.tail)
    case 4                  => copy(pc = pc + 2, output = output :+ r(1))
    case 5                  => copy(pc = if r(1) != 0 then r(2) else pc + 3)
    case 6                  => copy(pc = if r(1) == 0 then r(2) else pc + 3)
    case 7                  => copy(pc = pc + 4, memory = memory.updated(a(3), if r(1) < r(2) then 1 else 0))
    case 8                  => copy(pc = pc + 4, memory = memory.updated(a(3), if r(1) == r(2) then 1 else 0))
    case 9                  => copy(pc = pc + 2, relative = relative + r(1))

object Computer:
  def apply(program: String, input: Vector[Long] = Vector.empty, poke: Map[Long, Long] = Map.empty): Computer =
    new Computer(pc = 0L, memory = program.parse ++ poke, input, output = Vector.empty, relative = 0L)

extension (self: String)
  def parse: Map[Long, Long]   = self.longs.zipWithIndex.mapToMap(_.swap.lmap(_.toLong)).withDefaultValue(0L)
  def asciiLongs: Vector[Long] = self.toVector.map(_.toLong)
