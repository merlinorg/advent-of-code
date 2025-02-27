package org.merlin.aoc
package year2024
package day18

import lib.impl.IO.*
import scalaz.*
import Scalaz.*
import scala.collection.immutable.Queue

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: Vector[String] = loadv("sample.txt")

val actual: Vector[String] = loadv("actual.txt")

def part1(lines: Vector[String]): Long =
  val (size, take, walls) = parse(lines)
  solve(Search(walls, take, size), solution1)

def part2(lines: Vector[String]): Loc =
  val (size, take, walls) = parse(lines)
  Iterator
    .iterate(take + 1 -> walls.size): (i0, i1) =>
      if solve(Search(walls, i0 mid i1, size), solution2) then i0 -> (i0 mid i1) else (i0 mid i1) + 1 -> i1
    .findMap: (i0, i1) =>
      (i0 == i1).option(walls(i0 - 1))

def solve[A](init: Search, solution: Search => Option[A]): A =
  Iterator.iterate(init)(_.nextState).findMap(solution)

def solution1(search: Search): Option[Long] =
  Option.when(search.queue.head.loc == search.end)(search.queue.head.steps)

def solution2(search: Search): Option[Boolean] =
  Option.when(search.queue.isEmpty || search.queue.head.loc == search.end)(search.queue.isEmpty)

case class Search(walls: Set[Loc], end: Loc, size: Int, queue: Queue[State], visited: Set[Loc]):
  def nextState: Search =
    val (head, tail) = queue.dequeue
    val adjacent     = head.neighbours.filter(next => next.loc >=< size && !walls(next.loc) && !visited(next.loc))
    copy(queue = tail.enqueueAll(adjacent), visited = visited ++ adjacent.map(_.loc))

object Search:
  def apply(walls: Vector[Loc], take: Int, size: Int): Search =
    Search(walls.take(take).toSet, Loc(size - 1, size - 1), size, Queue(State(Origin, 0)), Set(Origin))

case class State(loc: Loc, steps: Long):
  def neighbours: Vector[State] =
    CardinalDirections.map(dir => State(loc + dir, steps + 1))

private def parse(lines: Vector[String]): (Int, Int, Vector[Loc]) = (
  if lines.length == 25 then 7 else 71,
  if lines.length == 25 then 12 else 1024,
  lines.collect:
    case s"${L(x)},${L(y)}" => Loc(x, y)
)
