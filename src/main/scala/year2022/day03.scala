package org.merlin.aoc
package year2022.day03

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
  input.parse
    .sumMap: (l, r) =>
      l.toSet.intersect(r.toSet).head.priority

def part2(input: String): Long =
  input.linesIterator
    .grouped(3)
    .sumMap: group =>
      group.map(_.toSet).reduce(_ intersect _).head.priority

extension (string: String)
  def parse: Iterator[(String, String)] = string.linesIterator.map: line =>
    line.splitAt(line.length / 2)

extension (char: Char) def priority: Long = if char < 'a' then char - 'A' + 27 else char - 'a' + 1
