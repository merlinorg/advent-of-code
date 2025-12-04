package org.merlin.aoc
package year2019.day08

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
  val best = input.parse.minBy(_.sumMap(_.countA('0')))
  best.sumMap(_.countA('1')) * best.sumMap(_.countA('2'))

def part2(input: String): String =
  val layers = input.parse.reverse
  layers.tail
    .foldLeft(layers.head): (layerBelow, layerAbove) =>
      layerBelow
        .zip(layerAbove)
        .map: (rowBelow, rowAbove) =>
          rowBelow.zip(rowAbove).map((below, above) => if above == '2' then below else above).mkString
    .mkString("\n")

extension (self: String)
  def parse: Vector[Vector[String]] =
    self
      .grouped(if self.length == 12 then 3 * 2 else 25 * 6)
      .toVector
      .map: layer =>
        layer.grouped(if self.length == 12 then 3 else 25).toVector
