package org.merlin.aoc
package year2021.day06

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

def part1(input: String): Long = solve(input.integers.toBag, 80)

def part2(input: String): Long = solve(input.integers.toBag, 256)

def solve(counts: Map[Int, Int], iter: Int): Long =
  Iterator
    .iterate((0 to 8).toList.map(counts.getOrElse(_, 0).toLong)):
      case (zero :: rest) :+ seven :+ eight =>
        rest ::: (seven + zero) :: eight :: zero :: Nil
      case never                            => never
    .nth(iter)
    .sum
