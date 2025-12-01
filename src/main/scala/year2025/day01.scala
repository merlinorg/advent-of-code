package org.merlin.aoc
package year2025.day01

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

def part1(input: String): Long =
  input.parse
    .scanLeft(50):
      case (pos, amt) => (pos + amt) %% 100
    .countA(0)

def part2(input: String): Long =
  input.parse
    .foldLeft(50 -> 0):
      case ((pos, total), amt) =>
        val next = (pos + amt) %% 100
        val zero = amt < 0 && pos != 0 && next > pos || amt > 0 && next < pos || next == 0
        next -> (total + amt.abs / 100 + (if zero then 1 else 0))
    ._2

extension (self: String)
  def parse: Vector[Int] =
    self.linesv.map: s =>
      s.tail.toInt * (if s.head == 'L' then -1 else 1)
