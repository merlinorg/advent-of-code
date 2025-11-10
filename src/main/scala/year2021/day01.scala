package org.merlin.aoc
package year2021.day01

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

def part1(input: String): Long =
  input.linesv.toLongs.slidingPairs.count:
    case (a, b) => b > a

def part2(input: String): Long =
  input.linesv.toLongs
    .sliding(3)
    .map(_.sum)
    .slidingPairs
    .count:
      case (a, b) => b > a
