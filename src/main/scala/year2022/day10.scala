package org.merlin.aoc
package year2022.day10

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
  execute(input).take(221).filter(c => c(0) % 40 == 20).sumMap(_.product)

def part2(input: String): String =
  val pixels = execute(input).map:
    case (cycle, x) => if (cycle % 40) >= x && (cycle % 40) < x + 3 then '#' else '.'
  pixels.grouped(40).map(_.mkString).take(6).mkString("\n")

def execute(input: String): Iterator[Vec2] =
  input.parse.scanLeft((1, 1)):
    case ((cycle, x), None)    => (cycle + 1, x)
    case ((cycle, x), Some(a)) => (cycle + 1, x + a)

extension (string: String)
  def parse: Iterator[Option[Int]] = string.linesIterator.flatMap:
    case s"addx ${I(a)}" => Iterator(None, Some(a))
    case _               => Iterator.single(None)
