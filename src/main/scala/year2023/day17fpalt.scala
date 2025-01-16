package org.merlin.aoc
package year2023
package day17fpalt

import lib.impl.IO.*
import scalaz.Scalaz.*
import scala.collection.SortedSet

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

private final case class Crucible(loc: Loc, dir: Dir, count: Int):
  def fwd: Crucible = copy(loc = loc + dir, count = count + 1)
  def cw: Crucible  = copy(loc = loc + dir.cw, dir = dir.cw, count = 1)
  def ccw: Crucible = copy(loc = loc + dir.ccw, dir = dir.ccw, count = 1)

  def moves(min: Int, max: Int): List[Crucible] =
    (count < max) ?? List(fwd) ++ (count >= min) ?? List(cw, ccw)

private given Ordering[Crucible] = Ordering.by(Tuple.fromProductTyped)

private final case class State(queue: SortedSet[(Int, Crucible)], seen: Set[Crucible]):
  def update(city: Board, min: Int, max: Int): State =
    val (loss, crucible) = queue.head

    val moves = for
      moved   <- crucible.moves(min, max) if moved.loc >=< city
      heatLoss = loss + city(moved.loc).asDigit if !seen.contains(moved)
    yield heatLoss -> moved

    State(queue.tail ++ moves, seen ++ moves.map(_._2))

  def solution(city: Board, min: Int): Option[Long] =
    queue.headOption.collect:
      case (heatLoss, crucible) if crucible.loc == city.se && crucible.count >= min => heatLoss

private def solve(city: Board, min: Int, max: Int): Long =
  Iterator
    .iterate(State(SortedSet(0 -> Crucible(city.nw, Dir.E, 0), 0 -> Crucible(city.nw, Dir.S, 0)), Set.empty)):
      state => state.update(city, min, max)
    .findMap(_.solution(city, min))

def part1(city: Board): Long = solve(city, 0, 3)

def part2(city: Board): Long = solve(city, 4, 10)
