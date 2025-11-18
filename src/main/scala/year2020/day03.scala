package org.merlin.aoc
package year2020.day03

import lib.{*, given}

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

def part1(input: String): Long = solve(input.linesv, 3 -> 1)

def part2(input: String): Long =
  val grid = input.linesv
  Vector((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
    .map: slope =>
      solve(grid, slope)
    .product

def solve(grid: Vector[String], slope: Vec2): Long =
  Iterator
    .iterate(Origin): loc =>
      ((loc.x + slope.x) %% grid.width, loc.y + slope.y)
    .takeWhile(_.y < grid.height)
    .count(grid(_) == '#')
