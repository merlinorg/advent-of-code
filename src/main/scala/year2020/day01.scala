package org.merlin.aoc
package year2020.day01

import lib.{*, given}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long =
  input.linesv.toLongs.allPairs.findMap: (a, b) =>
    Option.when(a + b == 2020)(a * b)

def part2(input: String): Long =
  input.linesv.toLongs
    .combinations(3)
    .findCollect:
      case Vector(a, b, c) if a + b + c == 2020 => a * b * c
