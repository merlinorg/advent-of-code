package org.merlin.aoc
package year2020.day08

import lib.{*, given}

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")

val actual: String = load("actual.txt")

def part1(input: String): Long =
  execute(input.parse)
    .findMap: (pc, acc, visited) =>
      Option.when(visited(pc))(acc)

def part2(input: String): Long =
  val program = input.parse
  program.indices.findMap: address =>
    val mutated      = program.updatedWith(address):
      case ("jmp", value) => "nop" -> value
      case ("nop", value) => "jmp" -> value
      case o              => o
    val (pc, acc, _) = execute(mutated)
      .findFirst: (pc, _, visited) =>
        visited(pc) || pc == program.size
    Option.when(pc == program.size)(acc)

def execute(program: Vector[(String, Int)]): Iterator[(Int, Long, Set[Int])] =
  Iterator
    .iterate((0, 0L, Set.empty[Int])): (pc, acc, visited) =>
      program(pc) match
        case ("jmp", value) => (pc + value, acc, visited + pc)
        case ("acc", value) => (pc + 1, acc + value, visited + pc)
        case _              => (pc + 1, acc, visited + pc)

extension (self: String)
  def parse: Vector[(String, Int)] =
    self.linesv.collect:
      case s"$op ${I(value)}" => op -> value
