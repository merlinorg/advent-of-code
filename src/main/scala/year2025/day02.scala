package org.merlin.aoc
package year2025.day02

import lib.{*, given}

import scala.collection.immutable.NumericRange

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
  input.parse.iterator.flatten
    .filter: id =>
      val string = id.toString
      val length = string.length
      string == string.take(length / 2) * 2
    .sum

// This tests just 800 candidates vs 2,000,000 for the naïve part1 above
def part1Alt(input: String): Long =
  val invalid = for
    range       <- input.parse
    digits      <- (range.min.digits + 1) / 2 to range.max.digits / 2
    modulus      = 10L ** digits
    multiplicand = modulus + 1
    lowest       = (range.min / multiplicand) max (modulus / 10)
    highest      = (range.max / multiplicand) min (modulus - 1)
    candidate   <- lowest to highest
    if range.contains(candidate * multiplicand)
  yield candidate * multiplicand
  invalid.sum

def part2(input: String): Long =
  input.parse.iterator.flatten
    .filter: id =>
      val string = id.toString
      val length = string.length
      (2 to length).exists: split =>
        string == string.take(length / split) * split
    .sum

def part2Alt(input: String): Long =
  input.parse.iterator.flatten.filter(id => Part2RE.matches(id.toString)).sum

val Part2RE = """(\d+)\1+""".r

extension (self: String)
  def parse: Vector[NumericRange[Long]] =
    self.commaSeparated.flatMap(LR.unapply)
