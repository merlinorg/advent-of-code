package org.merlin.aoc
package year2020.day13

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
  val (time, buses) = input.parse
  val id            = buses.values.minBy: period =>
    period - time % period
  id * (id - (time % id))

def part2(input: String): Long =
  val (_, buses) = input.parse
  buses.toVector
    .sortBy(s => -s._2)
    .foldLeft(0L -> 1L):
      case ((solution, totalPeriod), (offset, period)) =>
        val next = Iterator
          .iterate(solution)(_ + totalPeriod)
          .findFirst: candidate =>
            candidate % period == (period - offset) %% period
        next -> (totalPeriod * period)
    ._1

extension (self: String)
  def parse: (Long, Map[Int, Long]) =
    val lines = self.linesv
    lines(0).toLong -> lines(1)
      .split(',')
      .zipWithIndex
      .collectToMap:
        case (L(i), index) => index -> i
