package org.merlin.aoc
package year2023
package day04

import lib.impl.IO.{*, given}
import lib.legacy.*

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: Vector[String] = loadv("sample.txt")

val actual: Vector[String] = loadv("actual.txt")

private def wins(line: String): Int = line match
  case s"Card $_: $winning | $chosen" =>
    val wins = NumRe.findAllIn(winning).toSet
    val mine = NumRe.findAllIn(chosen).toSet
    wins.intersect(mine).size

def part1(lines: Vector[String]): Long =
  val scores = for
    line <- lines
    ok    = wins(line)
    if ok > 0
  yield 1 << (ok - 1)

  scores.sum

def part2(lines: Vector[String]): Long =
  val winMap             = lines.zipWithIndex.map(_.swap.rmap(wins)).toMap
  def count(i: Int): Int = 1 + (i + 1 to i + winMap(i)).map(count).sum
  lines.indices.map(count).sum
