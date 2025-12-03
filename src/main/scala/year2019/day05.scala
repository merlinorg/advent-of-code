package org.merlin.aoc
package year2019.day05

import lib.{*, given}

@main
def part1(): Unit =
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(actual))

val actual: String = load("actual.txt")

def part1(input: String): Long =
  compute(input.parse, 1).last

def part2(input: String): Long =
  compute(input.parse, 5).last

private def compute(memory: Map[Int, Int], input: Int): Vector[Int] =
  Iterator
    .iteropt((pc = 0, memory = memory, output = Vector.empty[Int])):
      case (pc, memory, output) =>
        val value = memory(pc)
        val op    = value % 100
        val r0    = if (value / 100) % 10 == 0 then memory(memory(pc + 1)) else memory(pc + 1)
        val r1    = if (value / 1000) % 10 == 0 then memory(memory(pc + 2)) else memory(pc + 2)
        if op == 1 || op == 2 then
          val result = if op == 1 then r0 + r1 else r0 * r1
          Some((pc = pc + 4, memory = memory.updated(memory(pc + 3), result), output = output))
        else if op == 3 then Some((pc = pc + 2, memory = memory.updated(memory(pc + 1), input), output = output))
        else if op == 4 then Some((pc = pc + 2, memory = memory, output = output :+ r0))
        else if op == 5 then Some((pc = if r0 != 0 then r1 else pc + 3, memory = memory, output = output))
        else if op == 6 then Some((pc = if r0 == 0 then r1 else pc + 3, memory = memory, output = output))
        else if op == 7 then
          Some((pc = pc + 4, memory = memory.updated(memory(pc + 3), if r0 < r1 then 1 else 0), output = output))
        else if op == 8 then
          Some((pc = pc + 4, memory = memory.updated(memory(pc + 3), if r0 == r1 then 1 else 0), output = output))
        else None
    .last()
    .output

extension (self: String) def parse: Map[Int, Int] = self.integers.zipWithIndex.mapToMap(_.swap).withDefaultValue(0)
