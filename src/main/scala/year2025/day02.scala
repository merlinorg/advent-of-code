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

def part1Alt(input: String): Long =
  val invalid = for
    range     <- input.parse
    digits    <- range.head.digits / 2 to range.last.digits / 2
    modulus    = 10L ** digits
    step       = modulus + 1
    candidate <- (modulus / 10) * step until modulus * step by step
    if range.contains(candidate)
  yield candidate
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
  input.parse.iterator.flatten.filter(id => "^(\\d+)\\1+$".r.matches(id.toString)).sum

extension (self: String)
  def parse: Vector[NumericRange[Long]] =
    self.commaSeparated.collect:
      case s"${L(left)}-${L(right)}" => left to right
