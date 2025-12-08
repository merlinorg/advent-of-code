package org.merlin.aoc
package year2024
package day22

import lib.{*, given}
import lib.fp.{*, given}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample2))
  println(part2(actual))

val sample: Vector[String]  = loadv("sample.txt")
val sample2: Vector[String] = loadv("sample2.txt")
val actual: Vector[String]  = loadv("actual.txt")

def part1(lines: Vector[String]): Long =
  lines.sumMap: line =>
    Iterator.iterate(line.toLong)(next).nth(2000)

def part2(lines: Vector[String]): Long =
  val sequenceTotals = lines.foldMap: line =>
    val windows = Iterator.iterate(line.toLong)(next).take(2000).map(_ % 10).sliding(5)
    windows.foldRight(Map.empty[(Long, Long, Long, Long), Long]):
      case (Seq(a, b, c, d, e), map) => map + ((b - a, c - b, d - c, e - c) -> e)
      case (_, map)                  => map
  sequenceTotals.values.max

def next(secret0: Long): Long =
  val secret1 = ((secret0 * 64) ^ secret0) % 16777216
  val secret2 = ((secret1 / 32) ^ secret1) % 16777216
  ((secret2 * 2048) ^ secret2) % 16777216
