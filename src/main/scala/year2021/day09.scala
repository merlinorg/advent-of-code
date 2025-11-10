package org.merlin.aoc
package year2021.day09

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
  val board = input.linesv
  board.gridIterator
    .filter: (loc, height) =>
      loc.neighbours.forall: neighbour =>
        board.get(neighbour).forall(_ > height)
    .sumMap: (_, height) =>
      1 + height.asDigit

def part2(input: String): Long =
  val board = input.linesv
  board.gridIterator
    .filter: (loc, height) =>
      loc.neighbours.forall: neighbour =>
        board.get(neighbour).forall(_ > height)
    .map: (loc, height) =>
      floodfill(
        (height, loc),
        (height, loc) =>
          loc.neighbours.flatMap: neighbour =>
            board.get(neighbour).filter(h => h != '9' && h > height).strengthR(neighbour)
      ).size.toLong
    .sorted
    .takeRight(3)
    .product
