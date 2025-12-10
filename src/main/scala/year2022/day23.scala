package org.merlin.aoc
package year2022.day23

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
  val (elves, _) = input.elferator.nth(10)
  val min        = elves.reduce(_ min _) // no, intellij
  val max        = elves.reduce(_ max _) // no, intellij
  (max - min + SouthEast).product - elves.size

def part2(input: String): Long =
  1 + input.elferator.slidingPairs.indexWhere:
    case ((elves1, _), (elves2, _)) => elves1 == elves2

extension (self: String)
  def elferator: Iterator[(Set[Vec2], Vector[Vec2])] =
    Iterator.iterate(parse -> Vector(North, South, West, East)):
      case (elves, dirs) =>
        val moves = elves.mapTo: loc =>
          AllDirections
            .exists(dir => elves(loc + dir))
            .flatOption:
              dirs.findMapOpt: dir =>
                (!elves(loc + dir) && !elves(loc + dir.ccw2) && !elves(loc + dir.cw2)).option(loc + dir)

        val counts = moves.foldLeft(Map.empty[Vec2, Int]):
          case (acc, (elf, loc)) =>
            acc.updatedWith(loc.getOrElse(elf)):
              case None    => Some(1)
              case Some(i) => Some(i + 1)

        val elves2 = moves.toVector.map: (elf, move) =>
          move.filter(loc => counts(loc) == 1).getOrElse(elf)

        (elves2.toSet, dirs.tail :+ dirs.head)

  def parse: Set[Vec2] = self.linesv.gridIterator.collectToSet:
    case (loc, '#') => loc
