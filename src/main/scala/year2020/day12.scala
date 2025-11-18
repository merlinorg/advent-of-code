package org.merlin.aoc
package year2020.day12

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
  val (loc, _) = input.parse.foldLeft(Origin -> East):
    case ((loc, dir), ('F', amount))                             =>
      (loc + (dir * amount), dir)
    case ((loc, dir), (chr, amount)) if chr == 'L' || chr == 'R' =>
      val nxt = (0 until amount by 90).foldLeft(dir): (dir, _) =>
        if chr == 'L' then dir.ccw else dir.cw
      (loc, nxt)
    case ((loc, dir), (chr, amount))                             =>
      (loc + Dir(chr) * amount, dir)
  Origin |-| loc

def part2(input: String): Long =
  val (loc, _) = input.parse.foldLeft(Origin -> (10, -1)):
    case ((ship, waypoint), ('F', amount))                             =>
      (ship + waypoint * amount, waypoint)
    case ((ship, waypoint), (chr, amount)) if chr == 'L' || chr == 'R' =>
      val rotated = (0 until amount by 90).foldLeft(waypoint): (pos, _) =>
        if chr == 'L' then pos.ccw else pos.cw
      (ship, rotated)
    case ((ship, waypoint), (chr, amount))                             =>
      (ship, waypoint + Dir(chr) * amount)
  Origin |-| loc

extension (self: String)
  def parse: Vector[(Char, Int)] =
    self.linesv.map: s =>
      s.head -> s.tail.toInt
