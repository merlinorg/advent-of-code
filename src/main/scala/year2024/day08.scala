package org.merlin.aoc
package year2024
package day08

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
  parse(board)
    .flatMap: locations =>
      locations.allPairs.flatMap: (a, b) =>
        Vector(a + (a - b), b + (b - a)).filter(_ >=< board)
    .toSet
    .size

def part2(board: Board): Long =
  parse(board)
    .flatMap: locations =>
      locations.allPairs.flatMap: (a, b) =>
        Iterator.iterate(a)(_ + (a - b)).takeWhile(_ >=< board) ++
          Iterator.iterate(b)(_ + (b - a)).takeWhile(_ >=< board)
    .toSet
    .size

private def parse(board: Board): Iterable[Vector[Loc]] =
  board.locations
    .filter(loc => Character.isLetterOrDigit(board(loc)))
    .groupMap(board(_))(identity)
    .values
