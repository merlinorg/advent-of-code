package org.merlin.aoc
package year2019.day03

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
  val (first, second) = input.parse.map1: wire =>
    wire.foldLeft((position = Origin, visited = Set.empty[Vec2])):
      case ((start, visited), (direction, length)) =>
        val end = start + direction * length
        (end, visited ++ (start + direction to end))
  (first.visited & second.visited).minMap(_.magnitude)

def part2(input: String): Long =
  val (first, second) = input.parse.map1: wire =>
    wire.foldLeft((position = Origin, visited = Map.empty[Vec2, Int], steps = 0)):
      case ((start, visited, steps), (direction, length)) =>
        val end = start + direction * length
        val places = (start + direction to end).zip(steps + 1 to steps + length).toMap
        (end, places ++ visited, steps + length)
  (first.visited.keySet & second.visited.keySet).minMap: loc =>
    first.visited(loc) + second.visited(loc)

extension (self: String)
  def parse: Pair[Vector[(direction: Vec2, length: Int)]] =
    self.linesv.pair.map1: wire =>
      wire.commaSeparated.map: s =>
        Dir(s.head) -> s.tail.toInt
