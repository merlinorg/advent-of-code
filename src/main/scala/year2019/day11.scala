package org.merlin.aoc
package year2019
package day11

import lib.{*, given}

@main def part1(): Unit =
  println(part1(actual))

@main def part2(): Unit =
  println(part2(actual))

val actual: String = load("actual.txt")

def part1(input: String): Long =
  solve(input, 0).size

def part2(input: String): String =
  solve(input, 1).filter(_._2 == 1).keys.toGrid.mkString("\n")

def solve(input: String, colour: Long): Map[Vec2, Long] =
  Iterator
    .iteropt((Computer(input), Origin, North, Map(Origin -> colour))):
      case (computer, loc, dir, map) =>
        val comp0 = computer.copy(input = Vector(map.getOrElse(loc, 0)))
        for
          (color, comp1)    <- comp0.runIO
          (rotation, comp2) <- comp1.runIO
          nextDir            = if rotation == 0 then dir.ccw else dir.cw
        yield (comp2, loc + nextDir, nextDir, map + (loc -> color))
    .last(_._4)
