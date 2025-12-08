package org.merlin.aoc
package year2025
package day04

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
  val rolls = input.linesv.gridIndices('@')
  rolls.count: loc =>
    loc.allNeighbours.count(rolls) < 4

def part2(input: String): Long =
  Iterator
    .unfold(input.linesv.gridIndices('@')): rolls =>
      rolls
        .partition(_.allNeighbours.count(rolls) < 4)
        .fold: (remove, retain) =>
          Option.when(remove.nonEmpty)(remove.size -> retain)
    .sum
