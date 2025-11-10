package org.merlin.aoc
package year2021.day05

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
  input
    .parse
    .filter: (v0, v1) =>
      v0.x == v1.x || v0.y == v1.y
    .flatMap: (v0, v1) =>
      v0 to v1
    .toBag
    .count(_._2 > 1)

def part2(input: String): Long =
  input
    .parse
    .flatMap: (v0, v1) =>
      v0 to v1
    .toBag
    .count(_._2 > 1)

extension (string: String)
  def parse: Vector[(Vec2, Vec2)] = string.linesv.collect:
    case s"${I(x0)},${I(y0)} -> ${I(x1)},${I(y1)}" => (x0, y0) -> (x1, y1)
