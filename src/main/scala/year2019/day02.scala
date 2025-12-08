package org.merlin.aoc
package year2019
package day02

import lib.{*, given}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long =
  compute(input, 12, 2)

def part2(input: String): Long =
  (0 to 99)
    .cross(0 to 99)
    .findMap: (noun, verb) =>
      Option.when(compute(input, noun, verb) == 19690720)(noun * 100 + verb)

private def compute(program: String, noun: Long, verb: Long): Long =
  Computer(program, poke = Map(1L -> noun, 2L -> verb)).run.last(_.memory(0))
