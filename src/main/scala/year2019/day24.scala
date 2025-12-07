package org.merlin.aoc
package year2019
package day24

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
  val grid = input.linesv
  val dim  = grid.dimensions
  Iterator
    .iterate((grid.gridIndices('#'), Set.empty[Set[Vec2]])): (bugs, visited) =>
      val newBugs = bugs
        .flatMap(_.neighbours)
        .filter: bug =>
          val count = bug.neighbours.count(bugs)
          !bugs(bug) && (bug >=< dim) && (count == 1 || count == 2)
      (bugs.filter(bug => bug.neighbours.count(bugs) == 1) ++ newBugs, visited + bugs)
    .findMap: (bugs, visited) =>
      Option.when(visited(bugs)):
        bugs.sumMap: (x, y) =>
          2L ** (y * dim.x + x)

def part2(input: String): Long =
  val grid = input.linesv
  Iterator
    .iterate(grid.gridIndices('#').map(0 *: _)): bugs =>
      val newBugs = bugs
        .flatMap(_.hyperNeighbours)
        .filter: bug =>
          val count = bug.hyperNeighbours.count(bugs)
          !bugs(bug) && (count == 1 || count == 2)
      bugs.filter(bug => bug.hyperNeighbours.count(bugs) == 1) ++ newBugs
    .nth(if input == sample then 10 else 200, _.size)

extension (self: Vec3)
  def hyperNeighbours: Iterable[Vec3] =
    val (level, x, y) = self
    Seq(
      if x == 0 then Seq((level - 1, 1, 2))
      else if x == 3 && y == 2 then (0 until 5).map(y => (level + 1, 4, y))
      else Seq((level, x - 1, y)),
      if x == 4 then Seq((level - 1, 3, 2))
      else if x == 1 && y == 2 then (0 until 5).map(y => (level + 1, 0, y))
      else Seq((level, x + 1, y)),
      if y == 0 then Seq((level - 1, 2, 1))
      else if y == 3 && x == 2 then (0 until 5).map(x => (level + 1, x, 4))
      else Seq((level, x, y - 1)),
      if y == 4 then Seq((level - 1, 2, 3))
      else if y == 1 && x == 2 then (0 until 5).map(x => (level + 1, x, 0))
      else Seq((level, x, y + 1))
    ).flatten.filter:
      case (_, x, y) => x >= 0 && y >= 0 && x < 5 && y < 5
