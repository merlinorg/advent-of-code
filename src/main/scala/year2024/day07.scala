package org.merlin.aoc
package year2024
package day07

import lib.impl.IO.{*, given}
import lib.legacy.{*, given}

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
  parse(lines).foldMap:
    case (total, numbers) =>
      def solvable(numbers: Vector[Long], sum: Long = 0): Boolean = numbers match
        case num +: rest => solvable(rest, sum + num) || solvable(rest, sum * num)
        case _           => sum == total
      solvable(numbers) ?? total

def part2(lines: Vector[String]): Long =
  parse(lines).foldMap:
    case (total, numbers) =>
      def solvable(numbers: Vector[Long], sum: Long = 0): Boolean = numbers match
        case num +: rest => solvable(rest, sum + num) || solvable(rest, sum * num) || solvable(rest, sum || num)
        case _           => sum == total
      solvable(numbers) ?? total

private def parse(lines: Vector[String]): Vector[(Long, Vector[Long])] =
  lines.map:
    case s"$a: $b" => a.toLong -> b.numbers

extension (self: Long) def ||(n: Long): Long = s"$self$n".toLong
