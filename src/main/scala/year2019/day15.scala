package org.merlin.aoc
package year2019
package day15

import lib.{*, given}
import lib.queue.*

import scala.collection.mutable

@main def part1(): Unit =
  println(part1(actual))

@main def part2(): Unit =
  println(part2(actual))

val actual: String = load("actual.txt")

def part1(input: String): Long =
  navigate(input).values.findMap: (output, steps) =>
    Option.when(output == 2L)(steps)

def part2(input: String): Long =
  val map   = navigate(input)
  val start = map.findMap:
    case (loc, (output, _)) => Option.when(output == 2L)(loc)
  Iterator
    .iteropt(Set(start)): locs =>
      val visit = locs.flatMap: loc =>
        loc.neighbours.filter: neighbour =>
          !locs(neighbour) && map(neighbour)._1 != 0L
      Option.when(visit.nonEmpty)(locs ++ visit)
    .size - 1

def navigate(input: String): Map[Vec2, (Long, Int)] =
  val visited = mutable.Map(Origin -> (0L, 0))

  Queue.unfoldU((Origin, Computer(input), 0, 0L)): (location, computer, steps, _) =>
    for
      (dir, input)    <- Vector(North -> 1, South -> 2, West -> 3, East -> 4)
      next             = location + dir
      if !visited.contains(next)
      (output, drone) <- computer.copy(input = Vector(input)).runIO
      _                = visited.update(next, (output, 1 + steps))
      if output != 0
    yield (next, drone, 1 + steps, output)

  visited.toMap
