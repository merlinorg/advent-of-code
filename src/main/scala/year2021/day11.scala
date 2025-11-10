package org.merlin.aoc
package year2021.day11

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

def part1(input: String): Long = flasherator(input.linesv).take(101).sumMap(_._2)

def part2(input: String): Long =
  val initial = input.linesv
  flasherator(initial).indexWhere((_, count) => count == initial.area)

def flasherator(initial: Vector[String]): Iterator[(Vector[String], Int)] =
  Iterator
    .iterate(initial -> 0): (grid, _) =>
      Iterator
        .iterate((grid, 0, Option.empty[Map[Vec2, Int]])): (grid, count, flash) =>
          val next    = grid.gridMap: (c, loc) =>
            (c + flash.cata(_.getOrElse(loc, 0), 1)).toChar
          val flashes = next.gridIterator.flatMap: (loc, c) =>
            Option.when(c > '9' && grid(loc) <= '9')(loc)
          (next, count + flashes.size, Some(flashes.flatMap(_.allNeighbours).toBag))
        .findMap: (grid, count, flash) =>
          flash.filter(_.isEmpty).as(grid.map(_.map(c => if c > '9' then '0' else c)) -> count)
