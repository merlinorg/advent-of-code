package org.merlin.aoc
package year2021.day14

import lib.{*, given}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long = solve(input, 10)

def part2(input: String): Long = solve(input, 40)

private def solve(input: String, iter: Int): Long =
  val (start, rules) = input.parse

  val result = Iterator
    .iterate((' ' +: start).slidingPairs.sumToMap(_ -> 1L)): counts =>
      counts.toVector
        .flatMap: (tuple, count) =>
          rules.get(tuple).cata(c => Seq((tuple._1, c) -> count, (c, tuple._2) -> count), Seq(tuple -> count))
        .sumToMap(identity)
    .nth(iter)

  val counts = result.sumToMap:
    case ((_, c), count) => c -> count

  counts.values.max - counts.values.min

extension (self: String)
  def parse: (Vector[Char], Map[(Char, Char), Char]) =
    self.linesv.bichunk.bimap(
      _.head.toVector,
      _.collectToMap:
        case s"$xy -> ${C(c)}" => ((xy(0), xy(1)), c)
    )
