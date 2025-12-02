package org.merlin.aoc
package year2019.day04

import lib.{*, given}

import scala.collection.immutable.NumericRange

@main
def part1(): Unit =
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(actual))

val actual: String = load("actual.txt")

def part1(input: String): Long =
  input.parse
    .map(_.toString)
    .count: s =>
      s.slidingPairs.exists(_ == _) && s.slidingPairs.forall(_ <= _)

def part2(input: String): Long =
  input.parse
    .map(_.toString)
    .count: s =>
      s.slidingPairs.forall(_ <= _) && s.indices.tail.exists: i =>
        (s(i) == s(i - 1)) && (i == 1 || s(i - 2) != s(i)) && (i == s.length - 1 || s(i + 1) != s(i))

extension (self: String)
  def parse: NumericRange[Long] = self match
    case s"${L(min)}-${L(max)}" => min to max
