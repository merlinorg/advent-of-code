package org.merlin.aoc
package year2024
package day02

import lib.{*, given}

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: Vector[String] = loadv("sample.txt")

val actual: Vector[String] = loadv("actual.txt")

def part1(lines: Vector[String]): Long =
  lines.map(_.longs).count(safe)

def part2(lines: Vector[String]): Long =
  lines.map(_.longs).count: numbers =>
    safe(numbers) || numbers.indices.exists: index =>
      safe(numbers.splice(index, 1))

private def safe(numbers: Vector[Long]): Boolean =
  (numbers == numbers.sorted || numbers == numbers.sorted.reverse) &&
    numbers.slidingPairs.forall: (a, b) =>
      (a |-| b) >=< (1, 4)
