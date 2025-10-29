package org.merlin.aoc
package year2023
package day11

import lib.impl.IO.{*, given}
import lib.legacy.{*, given}

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

type Point = (Long, Long)

extension (pt: Point) private infix def Δ(op: Point): Long = (pt._1 - op._1).abs + (pt._2 - op._2).abs

private def inflate(
  lines: Vector[Vector[Char]],
  factor: Long
): IndexedSeq[Long] =
  lines.scanLeft(0L): (offset, line) =>
    if !line.contains('#') then offset + factor else offset + 1

private def allStars(
  lines: Vector[String],
  factor: Long
): Vector[Point] =
  val xs = inflate(lines.transpose, factor)
  val ys = inflate(lines.map(_.toVector), factor)
  for
    (line, y) <- lines.zip(ys)
    (chr, x)  <- line.zip(xs)
    if chr == '#'
  yield x -> y

def part1(lines: Vector[String]): Long =
  allStars(lines, 2).combinations(2).toList.foldMap(v => v(0) Δ v(1))

def part2(lines: Vector[String]): Long =
  allStars(lines, 1_000_000).combinations(2).toList.foldMap(v => v(0) Δ v(1))
