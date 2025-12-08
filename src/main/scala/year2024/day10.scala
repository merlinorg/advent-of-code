package org.merlin.aoc
package year2024
package day10

import lib.io.{*, given}
import lib.legacy.{*, given}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: Vector[String] = loadv("sample.txt")
val actual: Vector[String] = loadv("actual.txt")

def part1(board: Board): Long =
  trails(board).sumMap(_.toSet.size)

def part2(board: Board): Long =
  trails(board).sumMap(_.size)

private def trails(board: Board): Vector[Vector[Loc]] =
  board
    .findAll('0')
    .map: loc =>
      Iterator
        .iterate('0'.toInt -> Vector(loc)):
          case (digit, locations) =>
            (digit + 1, locations.flatMap(_.adjacents.filter(board.is(_, digit + 1))))
        .findMap:
          case (digit, locations) =>
            Option.when(digit == '9')(locations)
