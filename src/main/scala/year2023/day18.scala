package org.merlin.aoc
package year2023
package day18

import lib.io.{*, given}
import lib.legacy.{*, given}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: Vector[String] = loadv("sample.txt")
val actual: Vector[String] = loadv("actual.txt")

extension (self: Dir.type) def byNum(i: Int): Dir = Dir.fromOrdinal((i + 1) % 4)

private def parse1(lines: Vector[String]): Vector[Vec] =
  lines.map { case s"$dir $count (#$_)" => Dir.byName(dir) * count.toInt }

private def parse2(lines: Vector[String]): Vector[Vec] =
  lines.map { case s"$_ $_ (#$color)" => Dir.byNum(color.takeRight(1).toInt) * Integer.parseInt(color.take(5), 16) }

private def area(vertices: Vector[Loc]): Long = // shoelace algorithm
  vertices.zip(vertices.tail).map((a, b) => a.x * b.y - b.x * a.y).sum.abs / 2

private def cellArea(path: Vector[Vec]): Long = // area of cell coordinates + perimeter / 2 + 1
  area(path.scanLeft(Origin)(_ + _)) + path.sumMap(_.magnitude) / 2 + 1

def part1(lines: Vector[String]): Long = lines |> parse1 |> cellArea

def part2(lines: Vector[String]): Long = lines |> parse2 |> cellArea
