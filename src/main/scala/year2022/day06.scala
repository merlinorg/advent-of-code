package org.merlin.aoc
package year2022.day06

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
  input.sliding(4).indexWhere(_.toSet.size == 4) + 4

def part2(input: String): Long =
  input.sliding(14).indexWhere(_.toSet.size == 14) + 14
