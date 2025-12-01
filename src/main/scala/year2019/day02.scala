package org.merlin.aoc
package year2019.day02

import lib.{*, given}

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(actual))

val sample: String = load("sample.txt")

val actual: String = load("actual.txt")

def part1(input: String): Long =
  compute(input.parse, 12, 2)

def part2(input: String): Long =
  (0 to 99)
    .cross(0 to 99)
    .findMap: (noun, verb) =>
      Option.when(compute(input.parse, noun, verb) == 19690720)(noun * 100 + verb)

private def compute(memory: Map[Int, Int], noun: Int, verb: Int): Int =
  Iterator
    .itercol((pc = 0, memory = memory.updated(1, noun).updated(2, verb))):
      case (pc, memory) if memory(pc) == 1 || memory(pc) == 2 =>
        val r0     = memory(memory(pc + 1))
        val r1     = memory(memory(pc + 2))
        val result = if memory(pc) == 1 then r0 + r1 else r0 * r1
        (pc = pc + 4, memory = memory.updated(memory(pc + 3), result))
    .last()
    .memory(0)

extension (self: String) def parse: Map[Int, Int] = self.integers.zipWithIndex.mapToMap(_.swap).withDefaultValue(0)
