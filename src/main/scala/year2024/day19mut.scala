package org.merlin.aoc
package year2024
package day19mut

import lib.{*, given}
import lib.memo.*

import scala.collection.mutable

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

def part1(lines: Vector[String]): Long =
  val (towels, patterns)              = parse(lines)
  def solve(pattern: String): Boolean =
    pattern.isEmpty || towels.flatMap(pattern.dropPrefix).exists(solve)
  patterns.count(solve)

def part2(lines: Vector[String]): Long =
  val (towels, patterns)           = parse(lines)
  val cache                        = mutable.Map("" -> 1L)
  def solve(pattern: String): Long = cache.memo(pattern):
    towels.flatMap(pattern.dropPrefix).sumMap(solve)
  patterns.sumMap(solve)

private def parse(lines: Vector[String]): (Vector[String], Vector[String]) =
  (lines(0).split(", ").toVector, lines.drop(2))
