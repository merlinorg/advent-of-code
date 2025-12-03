package org.merlin.aoc
package year2025.day03

import lib.{*, given}

import scala.annotation.tailrec

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
  input.linesv.sumMap: bank =>
    solve(bank, 2, 0L)

def part2(input: String): Long =
  input.linesv.sumMap: bank =>
    solve(bank, 12, 0L)

@tailrec def solve(bank: String, batteries: Int, total: Long): Long =
  if batteries == 0 then total
  else
    val best = bank.take(bank.length - batteries + 1).max
    solve(bank.drop(1 + bank.indexOf(best)), batteries - 1, total * 10 + best.asDigit)
