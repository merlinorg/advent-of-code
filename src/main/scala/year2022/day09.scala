package org.merlin.aoc
package year2022.day09

import lib.{*, given}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(sample2))
  println(part2(actual))

val sample: String = load("sample.txt")

val sample2: String = load("sample2.txt")
val actual: String = load("actual.txt")

def part1(input: String): Int =
  solve(input, 2)

def part2(input: String): Int =
  solve(input, 10)

def solve(input: String, length: Int): Int =
  val (_, visited) =
    input.parse.foldLeft((Vector.fill(length)(Origin), Set(Origin))):
      case ((knots, visited), dir) =>
        val knots2 = knots
          .scanLeft(Nowhere):
            case (z, h) if z == Nowhere => h + dir
            case (h, t)                 =>
              val delta = h - t
              if delta(0).abs <= 1 && delta(1).abs <= 1 then t
              else (t(0) + delta(0).sign, t(1) + delta(1).sign)
        (knots2.tail, visited + knots2.last)
  visited.size

extension (string: String)
  def parse: Iterator[Vec2] = string.linesIterator.flatMap:
    case s"${D(a)} ${I(b)}" => Iterator.fill(b)(a)
