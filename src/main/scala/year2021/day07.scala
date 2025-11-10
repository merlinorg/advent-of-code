package org.merlin.aoc
package year2021.day07

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
  solve(input.integers.toBag): (pos, index) =>
    (pos - index).abs

def part2(input: String): Long =
  solve(input.integers.toBag): (pos, index) =>
    (pos - index).abs * ((pos - index).abs + 1) / 2

def solve(counts: Map[Int, Int])(cost: (Int, Int) => Long): Long =
  (counts.keys.min to counts.keys.max).minMap: index =>
    counts
      .map: (pos, count) =>
        cost(pos, index) * count
      .sum
