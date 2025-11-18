package org.merlin.aoc
package year2020.day02

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
  input.parse.count: (range, chr, str) =>
    range.contains(str.countA(chr))

def part2(input: String): Long =
  input.parse.count: (range, chr, str) =>
    val a = str.get(range.min - 1).contains(chr)
    val b = str.get(range.max - 1).contains(chr)
    a && !b || b && !a

extension (self: String)
  def parse: Vector[(Range, Char, String)] =
    self.linesv.collect:
      case s"${I(min)}-${I(max)} ${C(chr)}: $str" => (min to max, chr, str)