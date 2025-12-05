package org.merlin.aoc
package year2019.day13

import lib.{*, given}
import year2019.Computer

@main
def part1(): Unit =
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(actual))

val actual: String = load("actual.txt")

def part1(input: String): Long =
  solve(Computer(input))(_ => Vector.empty).values.count(_ == 2)

def part2(input: String): Long =
  solve(Computer(input, poke = Map(0L -> 2L))): map =>
    for
      bx     <- map.find(_._2 == 4).map(_._1.x).toVector
      paddles = map.filter(_._2 == 3).map(_._1.x)
      if paddles.nonEmpty
      px      = paddles.sum / paddles.size
    yield if bx < px then -1 else if bx > px then 1 else 0
  .apply(-1 -> 0)

def solve(computer: Computer)(f: Map[Vec2, Long] => Vector[Long]): Map[Vec2, Long] =
  Iterator
    .iteropt(computer, Map.empty[Vec2, Long]):
      case (computer, map) =>
        val comp0 = computer.copy(input = f(map))
        for
          (x, comp1)    <- comp0.runIO
          (y, comp2)    <- comp1.runIO
          (tile, comp3) <- comp2.runIO
        yield (comp3, map + ((x.toInt, y.toInt) -> tile))
    .last(_._2)
