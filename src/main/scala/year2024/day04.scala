package org.merlin.aoc
package year2024
package day04

import lib.impl.IO.*
import scalaz.*
import Scalaz.*

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: Vector[String] = loadv("sample.txt")

val actual: Vector[String] = loadv("actual.txt")

def part1(board: Board): Long =
  board.locations.foldMap: loc =>
    Dir.values.count: dir =>
      "XMAS".zipWithIndex.forall:
        case (c, index) =>
          board.is(loc + dir * index, c)

def part2(board: Board): Long =
  board.locations.count: loc =>
    board.is(loc, 'A') &&
      2 == OrdinalDirections.count: dir =>
        board.is(loc + dir, 'M') && board.is(loc + dir.reverse, 'S')
