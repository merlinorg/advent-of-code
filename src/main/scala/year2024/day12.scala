package org.merlin.aoc
package year2024
package day12

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


def part1(board: Board): Long =
  parse(board).foldMap: region =>
    region.area * region.perimeter

def part2(board: Board): Long =
  parse(board).foldMap: region =>
    region.area * region.sides

type Region = Set[Loc]

private def parse(board: Board): Vector[Region] =
  board.locations.foldLeft(Vector.empty[Region]): (regions, loc) =>
    if regions.exists(_.contains(loc)) then regions
    else regions :+ floodfill(board, loc, Set.empty)

private def floodfill(board: Board, loc: Loc, region: Region): Region =
  loc.adjacents.foldLeft(region + loc): (region, adj) =>
    if region(adj) || !board.is(adj, loc) then region
    else floodfill(board, adj, region)

extension (region: Region)
  def area: Long      = region.size
  def perimeter: Long = region.foldMap(_.adjacents.count(adj => !region(adj)))
  def sides: Long     = region.foldMap: loc =>
    CardinalDirections.count: dir =>
      !region(loc + dir) && (!region(loc + dir.ccw) || region(loc + dir.ccw2))
