package org.merlin.aoc
package year2025
package day07

import lib.map.*
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

def part1(input: String): Long = solve(input).splits

def part2(input: String): Long = solve(input).tachyons.sumMap(_._2)

def solve(input: String): (tachyons: Map[Int, Long], splits: Long) =
  input.linesv
    .foldLeft((tachyons = Map.empty[Int, Long], splits = 0L)): (tachyons, line) =>
      line.zipWithIndex.foldLeft(tachyons):
        case (_, ('S', index))                  => (Map(index -> 1L), 0L)
        case ((tachyons, splits), ('^', index)) =>
          val qty = tachyons.getOrElse(index, 0L)
          (tachyons.plus(index - 1, qty).plus(index + 1, qty) - index, splits + qty.sign)
        case (tachyons, _)                      => tachyons
