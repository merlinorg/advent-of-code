package org.merlin.aoc
package yearYYYY.dayXX

import lib.*

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
  1

def part2(input: String): Long =
  2

extension (string: String)
  def parse: Iterator[String] = string.linesIterator
