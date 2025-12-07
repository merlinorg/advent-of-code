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
    .foldLeft((tachyons = Map.empty[Int, Long], splits = 0L)):
      case ((tachyons, splits), line) =>
        tachyons.foldLeft((if line.contains('S') then Map(line.indexOf('S') -> 1L) else tachyons, splits)):
          case ((acc, splits), (index, count)) =>
            if line(index) != '^' then (acc, splits)
            else (acc.plus(index - 1, count).plus(index + 1, count) - index, splits + 1)
