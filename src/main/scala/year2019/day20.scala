package org.merlin.aoc
package year2019
package day20

import lib.{*, given}
import lib.queue.*

import scala.collection.mutable

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(sample))
//  println(part2(actual))

val sample: String = load("sample.txt")

val actual: String = load("actual.txt")

def part1(input: String): Long =
  val grid                                  = input.linesv
  val (start, finish, shortcuts, distances) = analyze(grid)

  given Ordering[(Vec2, Long)] = Ordering.by(v => -v._2)
  val seen                     = mutable.Set.empty[Vec2]
  PriorityQueue.unfold(start -> 0L): (loc, steps) =>
    Either.when(loc == finish, steps):
      if seen.add(loc) then
        val walk = distances(loc).toVector
          .map: (dst, cost) =>
            dst -> (cost + steps)
        walk ++ shortcuts.get(loc).strengthR(1 + steps)
      else Vector.empty

def part2(input: String): Long = {
  val grid                                  = input.linesv
  val (start, finish, shortcuts, distances) = analyze(grid)
  val start3: Vec3                          = 0 *: start
  val finish3                               = 0 *: finish
  val width                                 = grid.width
  val height                                = grid.height
  def isOuter(loc: Vec2): Boolean           =
    loc.x < 4 || loc.y < 4 || loc.x >= width - 4 || loc.y >= height - 4

  given Ordering[(Vec3, Long)] = Ordering.by(v => -v._2)
  val seen                     = mutable.Set.empty[Vec3]
  PriorityQueue.unfold[(Vec3, Long), Long](start3 -> 0L): (loc3, steps) =>
    Either.when(loc3 == finish3, steps):
      if seen.add(loc3) then
        val level *: loc = loc3
        val walk         = distances(loc).toVector
          .map: (dst, cost) =>
            (level *: dst) -> (steps + cost)
        val shortcut     = for
          dst  <- shortcuts.get(loc)
          outer = isOuter(loc)
          if (level > 0 || !outer) && (level < 1000 || outer)
        yield ((level + (if outer then -1 else 1)) *: dst) -> (steps + 1)
        println(s"$steps: $loc3 - $walk - $shortcut")
        walk ++ shortcut
      else Vector.empty

  //  val grid                       = input.linesv
//  val (start, finish, shortcuts) = analyze(grid)
//  val start3                     = 0 *: start
//  val finish3                    = 0 *: finish
//  val width                      = grid.width
//  val height                     = grid.height
//
//  def isOuter(loc: Vec2): Boolean =
//    loc.x < 4 || loc.y < 4 || loc.x >= width - 4 || loc.y >= height - 4
//
//  shortestPath[Vec3](
//    start3,
//    _ == finish3,
//    loc3 =>
//      val level *: loc = loc3
//      val neighbours   = loc.neighbours.filter(grid.is(_, '.')).map(level *: _)
//      val shortcut     = for
//        dst <- shortcuts.get(loc)
//        outer = isOuter(loc)
//        if level > 0 || !outer
//      yield
//         (level + (if outer then -1 else 1)) *: dst
//
//
//      neighbours ++ shortcut
//  ).get.length - 1
}

def analyze(grid: Vector[String]): (Vec2, Vec2, Map[Vec2, Vec2], Map[Vec2, Map[Vec2, Int]]) =
  val portals = for
    (loc, chr) <- grid.gridIterator if chr.isUpper
    dot        <- loc.neighbours.find(grid.is(_, '.'))
    chr2       <- loc.neighbours.flatMap(grid.get).find(_.isUpper)
  yield Set(chr, chr2) -> dot

  val portalMap = portals.toMultimap
  val shortcuts = portalMap.values
    .filter(_.size == 2)
    .flatMap: v =>
      Vector(v(0) -> v(1), v(1) -> v(0))
    .toMap
  val dots      = portals.map(_._2).toSet
  val distances = dots.mapToMap: dot =>
    val besties = mutable.Map.empty[Vec2, Int]
    Queue.unfoldU(dot -> Set.empty[Vec2]): (loc, visited) =>
      if dots(loc) && loc != dot && !besties.contains(loc) then besties.update(loc, visited.size)
      loc.neighbours
        .filterNot(visited)
        .filter(grid.is(_, '.'))
        .map: neighbour =>
          neighbour -> (visited + loc)
    dot -> besties.toMap

  val start  = portalMap(Set('A')).head
  val finish = portalMap(Set('Z')).head
  (start, finish, shortcuts, distances)

extension (self: String)
  def parse: Vector[Long] = self.linesv.map: s =>
    ???
