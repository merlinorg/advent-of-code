package org.merlin.aoc
package year2022.day18

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

def part1(input: String): Long =
  val blocks = input.parse
  blocks.sumMap: block =>
    block.neighbours.count(b => !blocks(b))

def part2(input: String): Long =
  val blocks = input.parse
  val min    = blocks.reduce(_ min _)
  val max    = blocks.reduce(_ max _)
  val voids  = for
    x  <- min.x to max.x
    y  <- min.y to max.y
    z  <- min.z to max.z
    xyz = (x, y, z)
    if shortestPath(
      xyz,
      loc => loc.x == min.x || loc.x == max.x || loc.y == min.y || loc.y == max.y || loc.z == min.z || loc.z == max.z,
      loc => loc.neighbours.filterNot(blocks)
    ).isEmpty
  yield xyz
  val solid  = blocks ++ voids
  blocks.sumMap: block =>
    block.neighbours.count(b => !solid(b))

extension (string: String)
  def parse: Set[(Int, Int, Int)] = string.linesIterator.collectToSet:
    case s"${I(x)},${I(y)},${I(z)}" => (x, y, z)
