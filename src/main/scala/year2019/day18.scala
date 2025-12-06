package org.merlin.aoc
package year2019
package day18

import lib.{*, given}
import lib.queue.*

import scala.collection.mutable

@main
def part1(): Unit =
  println(part1(sample1))
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(sample2))
  println(part2(actual))

val sample1: String = load("sample1.txt")
val sample2: String = load("sample2.txt")
val actual: String  = load("actual.txt")

def part1(input: String): Long =
  solve(input.linesv, "@")

def part2(input: String): Long =
  solve(input.linesv.splitRobots, "1234")

private def solve(grid: Vector[String], origins: String): Long =
  val total = grid.gridChars.count(_.isLower)
  val dist  = distances(grid)

  given Ordering[(Set[Char], Set[Char], Long, Set[Char])] = Ordering.by(x => (-x._3, x._2.size))

  val seen = mutable.Set.empty[(Set[Char], Set[Char])]
  PriorityQueue.unfold((origins.toSet, Set.empty[Char], 0L, Set.empty[Char])): (locs, keys, steps, visited) =>
    Either.when(keys.size == total, steps):
      val unseen = seen.add(locs, keys)
      locs.toSeq.flatMap: loc =>
        dist(loc)
          .filter: (c, _) =>
            unseen && !visited(c) && (!c.isUpper || keys(c.toLower))
          .map: (c, dist) =>
            if c.isLower && !keys(c) then (locs - loc + c, keys + c, steps + dist, Set.empty)
            else (locs - loc + c, keys, steps + dist, visited + c)

// The distances from each symbol to its connected symbols
private def distances(grid: Vector[String]): Map[Char, Map[Char, Int]] =
  grid.gridIterator
    .filter:
      case (_, chr) => chr != '.' && chr != '#'
    .mapToMap: (loc, chr) =>
      val besties = mutable.Map.empty[Char, Int]
      Queue.unfoldU(loc -> Set.empty[Vec2]): (loc, visited) =>
        loc.neighbours
          .filterNot(visited)
          .filter: neighbour =>
            val c = grid(neighbour)
            if c != '.' && c != '#' && c != chr && !besties.contains(c) then besties.update(c, 1 + visited.size)
            c == '.'
          .map: neighbour =>
            neighbour -> (visited + loc)
      chr -> besties.toMap

extension (self: Vector[String])
  def splitRobots: Vector[String] =
    val start = self.gridIndex('@')
    Vector(
      start             -> '#',
      start + North     -> '#',
      start + East      -> '#',
      start + South     -> '#',
      start + West      -> '#',
      start + NorthEast -> '1',
      start + SouthEast -> '2',
      start + SouthWest -> '3',
      start + NorthWest -> '4',
    ).foldLeft(self):
      case (grid, (loc, char)) => grid.updated(loc, char)
