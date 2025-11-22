package org.merlin.aoc
package year2020.day05

import lib.{*, given}

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main
def part2(): Unit =
//  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")

val actual: String = load("actual.txt")

def part1(input: String): Long =
  input.parse.max

def part2(input: String): Long =
  val seats = input.parse.toSet
  (seats.min to seats.max).findFirst: s =>
    seats(s - 1) && seats(s + 1) && !seats(s)

extension (self: String)
  def parse: Vector[Int] =
    self.linesv.map: s =>
      s.foldLeft(0): (sn, chr) =>
        sn * 2 + (if chr == 'B' || chr == 'R' then 1 else 0)
