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
  Computer(0, memory.updated(1, noun).updated(2, verb)).run.last().memory(0)

case class Computer(pc: Int, memory: Map[Int, Int]):
  def run: Iterator[Computer] = Iterator.iteropt(this)(_.step)

  def r(i: Int): Int = memory(memory(pc + i))

  def step: Option[Computer] = PartialFunction.condOpt(memory(pc) % 100):
    case 1 => copy(pc = pc + 4, memory = memory.updated(memory(pc + 3), r(1) + r(2)))
    case 2 => copy(pc = pc + 4, memory = memory.updated(memory(pc + 3), r(1) * r(2)))

object Computer:
  def apply(memory: Map[Int, Int]): Computer = new Computer(0, memory)

extension (self: String) def parse: Map[Int, Int] = self.integers.zipWithIndex.mapToMap(_.swap).withDefaultValue(0)
