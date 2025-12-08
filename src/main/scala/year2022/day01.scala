package org.merlin.aoc
package year2022.day01

import lib.{*, given}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long =
  input.linesv.chunks.map(_.toLongs.sum).max

def part2(input: String): Long =
  input.linesv.chunks.map(_.toLongs.sum).sorted.takeRight(3).sum
