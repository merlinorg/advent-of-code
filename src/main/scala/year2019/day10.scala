package org.merlin.aoc
package year2019.day10

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
  val asteroids = input.parse
  asteroids.maxMap: asteroid =>
    asteroids.visibleFrom(asteroid).size

def part2(input: String): Long =
  val asteroids = input.parse
  val best      = asteroids.maxBy: asteroid =>
    asteroids.visibleFrom(asteroid).size
  Iterator
    .unfold(asteroids -> Seq.empty[Vec2]): (asteroids, queue) =>
      val obliterate =
        if queue.nonEmpty then queue
        else
          asteroids
            .visibleFrom(best)
            .toSeq
            .sortBy: asteroid =>
              -Math.atan2(asteroid.x - best.x, asteroid.y - best.y)
      obliterate.headOption.map: asteroid =>
        asteroid -> (asteroids - asteroid, obliterate.tail)
    .nth(199)
    .fold(_ * 100 + _)

extension (self: Set[Vec2])
  def visibleFrom(asteroid: Vec2): Iterable[Vec2] =
    (self - asteroid)
      .foldLeft(Map.empty[Vec2, Vec2]): (acc, other) =>
        val delta = asteroid - other
        val gcd   = delta.x gcd delta.y
        acc.updatedWith(delta / gcd.abs): existing =>
          existing.filter(o => (o - asteroid).magnitude < delta.magnitude).orElse(Some(other))
      .values

extension (self: String)
  def parse: Set[Vec2] = self.linesv.gridIterator.collectToSet:
    case (loc, '#') => loc
