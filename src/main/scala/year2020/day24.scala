package org.merlin.aoc
package year2020.day24

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
  solve1(input.linesv).size

def part2(input: String): Long =
  Iterator
    .iterate(solve1(input.linesv)): tiles =>
      def count(loc: Vec2): Int = loc.adjacents.count(tiles)
      val flip                  = tiles.flatMap(_.adjacents).filterNot(tiles).filter(count(_) == 2)
      val flop                  = tiles.filter(tile => count(tile) < 1 || count(tile) > 2)
      tiles -- flop ++ flip
    .nth(100)
    .size

def solve1(rules: Vector[String]): Set[Vec2] =
  rules
    .foldLeft(Set.empty[Vec2]): (set, str) =>
      val loc = DirRe
        .findAllIn(str)
        .foldLeft(Origin): (loc, dir) =>
          loc + TileDir(dir)
      if set(loc) then set - loc else set + loc

extension (self: Vec2) def adjacents: Iterable[Vec2] = TileDir.values.map(self + _)

val DirRe = "e|se|sw|w|nw|ne".r

val TileDir = Map(
  "e"  -> East,
  "w"  -> West,
  "ne" -> North,
  "se" -> SouthEast,
  "nw" -> NorthWest,
  "sw" -> South
)
