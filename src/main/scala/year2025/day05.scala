package org.merlin.aoc
package year2025.day05

import lib.{*, given}
import lib.interval.*

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
  val (fresh, ingredients) = input.parse
  ingredients.count(fresh.contains)

def part2(input: String): Long =
  val (fresh, _) = input.parse
  fresh.foldLeft(Set.empty[Interval])(_ union _).sumMap(_.range)

extension (self: String)
  def parse: (Set[Interval], Vector[Long]) = self.linesv.bichunk.bimap(
    _.flatMap(Interval.unapply).toSet,
    _.map(_.toLong)
  )
