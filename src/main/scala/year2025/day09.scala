package org.merlin.aoc
package year2025
package day09

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
  input.parse.allPairs.maxMap(_.area)

// If any line intersects the rect I exclude it, but that's not really lawful,
// there could be two adjacent parallel lines that intersect a valid rectangle
// but there aren't any, the shortest edge is 5 long.
def part2(input: String): Long =
  val corners  = input.parse
  val allEdges = (corners :+ corners.head).slidingPairs
  corners.allPairs
    .sortBy(r => -r.area)
    .findMap: rect =>
      Option.when(!allEdges.exists(_.intersects(rect)))(rect.area)

extension (self: (Vec2, Vec2))
  def area: Long = (1 + (self(0).x - self(1).x).abs) *< (1 + (self(0).y - self(1).y).abs)

  // touching is okay, lines passing through each other are not
  def intersects(rect: (Vec2, Vec2)): Boolean =
    val (rx0, ry0) = rect.fold(_ min _)
    val (rx1, ry1) = rect.fold(_ max _)
    val (lx0, ly0) = self.fold(_ min _)
    val (lx1, ly1) = self.fold(_ max _)
    lx0 < rx1 && lx1 > rx0 && ly0 < ry1 && ly1 > ry0

extension (self: String) def parse: Vector[Vec2] = self.linesv.flatMap(Vec2.unapply)
