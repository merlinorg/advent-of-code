package org.merlin.aoc
package year2020.day06

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
  input.linesv.chunks.sumMap: chunk =>
    chunk
      .foldLeft(Set.empty[Char]): (set, str) =>
        set ++ str
      .size

def part2(input: String): Long =
  input.linesv.chunks.sumMap: chunk =>
    chunk
      .foldLeft(('a' to 'z').toSet): (set, str) =>
        set.intersect(str.toSet)
      .size
