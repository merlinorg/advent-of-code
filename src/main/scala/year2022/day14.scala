package org.merlin.aoc
package year2022.day14

import lib.*

import scala.annotation.tailrec

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
  val (set, maxY) = input.parse
  Iterator
    .iteropt(set): set =>
      @tailrec def loop(pos: Vec2): Option[Vec2] = SandFlow.find(dir => !set(pos + dir)) match
        case _ if pos.y == maxY => None
        case Some(dir)          => loop(pos + dir)
        case None               => Some(pos)
      loop(SandEntry).map(set + _)
    .length - 1

def part2(input: String): Long =
  val (set, maxY) = input.parse
  Iterator
    .iteropt(set): set =>
      @tailrec def loop(pos: Vec2): Vec2 = SandFlow.find(dir => !set(pos + dir) && pos.y < maxY + 1) match
        case Some(dir) => loop(pos + dir)
        case None      => pos
      set(SandEntry).noption:
        set + loop(SandEntry)
    .length - 1

val SandEntry = (500, 0)
val SandFlow  = Vector(South, SouthWest, SouthEast)

extension (string: String)
  def parse: (Set[Vec2], Int) =
    val paths = string.linesIterator.map(_.integers.pairs)
    val set   = paths.foldLeft(Set.empty[Vec2]):
      case (set, path) =>
        path.slidingPairs.foldLeft(set):
          case (set, (a, b)) => set ++ (a to b)
    (set, set.map(_.y).max)
