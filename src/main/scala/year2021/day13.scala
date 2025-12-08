package org.merlin.aoc
package year2021.day13

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
  val (points, rules) = input.parse
  solve(points, rules.take(1)).size

def part2(input: String): String =
  val (points, rules) = input.parse
  val result          = solve(points, rules)
  result.toGrid.mkString("\n")

def solve(points: Set[Vec2], rules: Vector[Vec2]): Set[Vec2] =
  rules.foldLeft(points): (points, rule) =>
    points.map: point =>
      if rule.y == 0 then if point.x > rule.x then (2 * rule.x - point.x, point.y) else point
      else if point.y > rule.y then (point.x, 2 * rule.y - point.y)
      else point

extension (self: String)
  def parse: (Set[Vec2], Vector[Vec2]) =
    self.linesv.bichunk.bimap(
      _.collectToSet:
        case s"${I(x)},${I(y)}" => (x, y)
      ,
      _.collect:
        case s"fold along x=${I(x)}" => (x, 0)
        case s"fold along y=${I(y)}" => (0, y)
    )
