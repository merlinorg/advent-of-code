package org.merlin.aoc
package year2024
package day11

import lib.fp.{*, given}
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

def part1(lines: Vector[String]): Long = blink(lines, 25)

def part2(lines: Vector[String]): Long = blink(lines, 75)

private def blink(lines: Vector[String], iterations: Int): Long =
  Iterator
    .iterate(lines.head.longs.foldMap(n => Map(n -> 1L))): kenneth =>
      kenneth.toVector.foldMap:
        case (0, count)          => Map(1L -> count)
        case (Even(a, b), count) => if a == b then Map(a -> 2 * count) else Map(a -> count, b -> count)
        case (n, count)          => Map(n * 2024 -> count)
    .nth(iterations)
    .suml

object Even:
  def unapply(value: Long): Option[(Long, Long)] =
    Option(value.digits).filter(_.even).map(digits => value /% (10L ** (digits / 2)))
