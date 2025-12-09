package org.merlin.aoc
package year2025
package day01

import lib.{*, given}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Int = solve1(input.parse)

given IntMod = IntMod(100)

private def solve1(rotations: Vector[Int]): Int =
  rotations
    .scanLeft(50):
      case (position, amount) => position +%% amount
    .countA(0)

def part2Alt(input: String): Int =
  solve1(input.parse.flatMap(amount => Seq.fill(amount.abs)(amount.sign)))

def part2(input: String): Int =
  input.parse
    .foldLeft((position = 50, zeroes = 0)):
      case ((position, zeroes), amount) =>
        val next = position +%% amount
        val zero = next == 0 || amount > 0 && next < position || amount < 0 && position != 0 && next > position
        (position = next, zeroes = zeroes + amount.abs / 100 + (if zero then 1 else 0))
    .zeroes

extension (self: String)
  def parse: Vector[Int] =
    self.linesv.collect:
      case s"L${I(left)}"  => -left
      case s"R${I(right)}" => right
