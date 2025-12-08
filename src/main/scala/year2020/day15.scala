package org.merlin.aoc
package year2020.day15

import lib.{*, given}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long = solve(input.integers, 2020)

def part2(input: String): Long = solve(input.integers, 30000000)

def solve(numbers: Vector[Int], iter: Int): Long =
  Iterator
    .iterate((Map.empty[Int, Int], numbers.zipWithIndex.toMap, numbers.last, numbers.size)):
      (prior, recent, previous, loop) =>
        val next = if prior.contains(previous) then recent(previous) - prior(previous) else 0
        (
          recent.get(next).cata(l => prior + (next -> l), prior),
          recent + (next -> loop),
          next,
          1 + loop
        )
    .findMap: (_ ,_, previous, loop) =>
      Option.when(loop == iter)(previous)
