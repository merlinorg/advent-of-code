package org.merlin.aoc
package year2023
package day21

import lib.impl.IO.*
import scalaz.*
import Scalaz.*

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

final case class Problem1FSM(
  board: Board,
  locations: Set[Loc]
):
  def nextState: Problem1FSM =
    val nextLocations = for
      loc <- locations
      adj <- loc.adjacents
      if adj >=< board && board(adj) != '#'
    yield adj
    copy(locations = nextLocations)

end Problem1FSM

def part1(board: Board): Long =
  Iterator
    .iterate(Problem1FSM(board, Set(board.find('S'))))(_.nextState)
    .drop(if (board.length == 11) 6 else 64)
    .next()
    .locations
    .size

final case class Problem2FSM(
  board: Board,
  index: Int,
  locations: Set[Loc]
):
  def nextState: Problem2FSM =
    val nextLocations = for
      loc <- locations
      adj <- loc.adjacents
      if board(adj.y.toInt %% board.length)(adj.x.toInt %% board.length) != '#'
    yield adj
    copy(index = index + 1, locations = nextLocations)

end Problem2FSM

final case class Solution2FSM(
  steps: Long,
  size: Long,
  solutions: Vector[Long]
):
  private val quotient = steps / size
  private val modulus  = steps % size

  inline def +(fsm: Problem2FSM): Solution2FSM =
    if fsm.index % size == modulus then
      println(s"${fsm.index} -> ${fsm.locations.size}")
      copy(solutions = solutions :+ fsm.locations.size)
    else this

  def solution: Option[Long] = solutions match
    case Vector(y0, y1, y2) =>
      (0 until 4).foreach: x =>
        println(s"$x -> ${solve(x, y0, y1, y2)}")
      Some(solve(quotient, y0, y1, y2))

    case _ => None
  end solution

  private def solve(x: Long, y0: Long, y1: Long, y2: Long): Long =
    y0 + (y1 - y0) * x + (x * (x - 1) / 2) * (y2 - 2 * y1 + y0)

end Solution2FSM

private def part2Real(board: Board): Long =
  Iterator
    .iterate(Problem2FSM(board, 0, Set(board.find('S'))))(_.nextState)
    .scanLeft(Solution2FSM(26501365, board.length, Vector.empty))(_ + _)
    .findMap(_.solution)

private def part2Pretend(board: Board): Long =
  Iterator
    .iterate(Problem2FSM(board, 0, Set(board.find('S'))))(_.nextState)
    .tapEach: fsm =>
      if (fsm.index == 6 || fsm.index == 10 || fsm.index == 50 || fsm.index == 100)
        println(s"${fsm.index} -> ${fsm.locations.size}")
    .drop(500)
    .next()
    .locations
    .size

def part2(board: Board): Long =
  if board.length == 11 then part2Pretend(board) else part2Real(board)
