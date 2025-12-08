package org.merlin.aoc
package year2022.day12

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
  val (grid, start, end) = input.parse
  solve(grid, start, end).get

def part2(input: String): Long =
  val (grid, _, end) = input.parse
  grid.gridIterator.minCollect:
    case (xy, 'a') =>
      solve(grid, xy, end).getOrElse(Int.MaxValue)

def solve(grid: Vector[String], start: Vec2, end: Vec2): Option[Int] =
  shortestPath(
    start,
    _ == end,
    pos =>
      pos.neighbours.filter: neighbour =>
        grid.get(neighbour).exists(_ - grid(pos) <= 1)
  ).map(_.length - 1)

extension (string: String)
  def parse: (Vector[String], Vec2, Vec2) =
    val grid  = string.linesv
    val start = grid.gridIndex('S')
    val end   = grid.gridIndex('E')
    (grid.updated(start, 'a').updated(end, 'z'), start, end)
