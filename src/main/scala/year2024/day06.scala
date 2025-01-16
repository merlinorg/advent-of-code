package org.merlin.aoc
package year2024
package day06

import lib.impl.IO.*
import scalaz.*
import Scalaz.*
import scala.collection.parallel.CollectionConverters.*

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

def part1(board: Board): Long =
  Iterator.iterate(Guard1FSM(board))(_.next).findMap(_.solution)

case class Guard1FSM(loc: Loc, dir: Dir, board: Board, visited: Set[Loc]):
  def solution: Option[Long] = (loc <>= board).option(visited.size)

  def next: Guard1FSM =
    val nxt = loc + dir
    if (board.is(nxt, '#'))
      copy(dir = dir.cw)
    else
      copy(loc = nxt, visited = visited + loc)

object Guard1FSM:
  def apply(board: Board): Guard1FSM =
    Guard1FSM(board.loc('^'), Dir.N, board, Set.empty)

def part2(board: Board): Long =
  board.locations.par.count: obstacle =>
    board.is(obstacle, '.') && Iterator.iterate(Guard2FSM(board, obstacle))(_.next).findMap(_.solution)

case class Guard2FSM(loc: Loc, dir: Dir, board: Board, obstacle: Loc, visited: Set[(Loc, Dir)]):
  def solution: Option[Boolean] =
    val looped = visited.contains(loc -> dir)
    (looped || (loc <>= board)).option(looped)

  def next: Guard2FSM =
    val nxt = loc + dir
    if (board.is(nxt, '#') || nxt == obstacle)
      copy(dir = dir.cw, visited = visited + (loc -> dir))
    else
      copy(loc = nxt, visited = visited + (loc -> dir))

object Guard2FSM:
  def apply(board: Board, obstacle: Loc): Guard2FSM =
    Guard2FSM(board.loc('^'), Dir.N, board, obstacle, Set.empty)

