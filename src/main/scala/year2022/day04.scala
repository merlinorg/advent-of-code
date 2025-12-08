package org.merlin.aoc
package year2022.day04

import lib.{*, given}

import scala.collection.immutable.NumericRange

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long =
  input.parse
    .count: (l, r) =>
      l.contains(r.head) && l.contains(r.last) || r.contains(l.head) && r.contains(l.last)

def part2(input: String): Long =
  input.parse
    .count: (l, r) =>
      l.contains(r.head) || l.contains(r.last) || r.contains(l.head) || r.contains(l.last)

extension (string: String)
  def parse: Iterator[Pair[NumericRange[Long]]] = string.linesIterator.collect:
    case s"${LR(a)},${LR(b)}" => (a, b)
