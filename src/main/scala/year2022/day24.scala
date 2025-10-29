package org.merlin.aoc
package year2022.day24

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
  val map    = input.linesv
  val start  = (1, 0)
  val finish = (map.width - 2, map.height - 1)

  solve(map, start, map.gales)
    .takeUntil: (positions, _) =>
      positions.contains(finish)
    .length - 1

def part2(input: String): Long =
  val map    = input.linesv
  val start  = (1, 0)
  val finish = (map.width - 2, map.height - 1)

  val (count1, gales1) = solve(map, start, map.gales).zipWithIndex.findMap:
    case ((positions, gales), index) =>
      positions(finish).option(index -> gales)

  val (count2, gales2) = solve(map, finish, gales1).zipWithIndex.findMap:
    case ((positions, gales), index) =>
      positions(start).option(index -> gales)

  solve(map, start, gales2).zipWithIndex.findMap:
    case ((positions, _), index) =>
      positions(finish).option(index + count1 + count2)

def solve(map: Vector[String], loc: Vec2, gales: Gales): Iterator[(Set[Vec2], Gales)] =
  Iterator.iterate(Set(loc) -> gales):
    case (positions, gales) =>
      val gales2     = map.blow(gales)
      val positions2 = positions.flatMap: loc =>
        (CardinalDirections.map(dir => loc + dir) :+ loc).filter: loc =>
          map.get(loc).exists(_ != '#') && !gales2.contains(loc)
      positions2 -> gales2

type Gales = Map[Vec2, Vector[Vec2]]

extension (self: Vector[String])
  def gales: Gales = self.gridIterator
    .flatMap:
      case (c, loc) => Dir.get(c).map(dir => loc -> Vector(dir))
    .toMap

  def step(loc: Vec2, dir: Vec2): Vec2 = loc + dir match
    case (0, y)                         => (self.width - 2, y)
    case (x, y) if x == self.width - 1  => (1, y)
    case (x, 0)                         => (x, self.height - 2)
    case (x, y) if y == self.height - 1 => (x, 1)
    case o                              => o

  def blow(gales: Gales): Gales =
    gales.foldLeft(Map.empty[Vec2, Vector[Vec2]]):
      case (acc, (loc, dirs)) =>
        dirs.foldLeft(acc):
          case (acc, dir) =>
            acc.updatedWith(step(loc, dir)):
              case None    => Some(Vector(dir))
              case Some(v) => Some(v :+ dir)
