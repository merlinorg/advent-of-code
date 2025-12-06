package org.merlin.aoc
package year2019
package day20

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

val actual: String = load("actual.txt")

def part1(input: String): Long =
  val grid                       = input.linesv
  val (start, finish, distances) = analyze(grid)

  PriorityQueue.unfold((loc = start, steps = 0L))(using Priority.least(_.steps, _.loc)):
    case (loc, steps) =>
      Either.when(loc == finish)(steps):
        distances(loc).map(_.rmap(_.steps + steps))

def part2(input: String): Long =
  val grid                       = input.linesv
  val (start, finish, distances) = analyze(grid)

  PriorityQueue.unfold((loc = 0 *: start, steps = 0L))(using Priority.least(_.steps, _.loc)):
    case (level *: loc, steps) =>
      Either.when(loc == finish && level == 0)(steps):
        distances(loc)
          .map: (dst, cost) =>
            ((level + cost.levels) *: dst) -> (steps + cost.steps)
          .filter(t => t._1._1 >= 0)

type Cost = (steps: Int, levels: Int)

def analyze(grid: Vector[String]): (Vec2, Vec2, Map[Vec2, Vector[(Vec2, Cost)]]) =
  val (width, height) = grid.dimensions

  val portals = for
    (loc, chr) <- grid.gridIterator if chr.isUpper
    dot        <- loc.neighbours.find(grid.is(_, '.'))
    chr2       <- loc.neighbours.flatMap(grid.get).find(_.isUpper)
  yield Set(chr, chr2) -> dot

  val portalMap = portals.toMultimap
  val shortcuts = portalMap.values
    .flatMap: v =>
      Vector(v.head -> v.last, v.last -> v.head).filter(_ != _)
    .toMap
  val dots      = portals.map(_._2).toSet
  val distances = dots.mapToMap: dot =>
    val besties = mutable.Map.empty[Vec2, Cost]
    shortcuts
      .get(dot)
      .foreach: worm =>
        val outside = dot.x < 4 || dot.y < 4 || dot.x >= width - 4 || dot.y >= height - 4
        besties.update(worm, 1 -> (if outside then -1 else 1))
    Queue.unfoldU(dot -> Set.empty[Vec2]): (loc, visited) =>
      if dots(loc) && loc != dot && !besties.contains(loc) then besties.update(loc, visited.size -> 0)
      loc.neighbours
        .filterNot(visited)
        .filter(grid.is(_, '.'))
        .map: neighbour =>
          neighbour -> (visited + loc)
    dot -> besties.toVector

  val start  = portalMap(Set('A')).head
  val finish = portalMap(Set('Z')).head
  (start, finish, distances)
