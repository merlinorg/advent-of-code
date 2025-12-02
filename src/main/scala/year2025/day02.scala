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
  input.parse.flatten
    .filter: id =>
      val string = id.toString
      val length = string.length
      string == string.take(length / 2) * 2
    .sum

def part2(input: String): Long =
  input.parse.flatten
    .filter: id =>
      val string = id.toString
      val length = string.length
      (2 to length).exists: split =>
        string == string.take(length / split) * split
    .sum

extension (self: String)
  def parse: Vector[NumericRange[Long]] =
    self.commaSeparated.collect:
      case s"${L(left)}-${L(right)}" => left to right
