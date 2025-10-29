package org.merlin.aoc
package year2023
package day06

import lib.impl.IO.{*, given}
import lib.legacy.*

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

def part1(lines: Vector[String]): Long =
  val times     = NumRe.findAllIn(lines.head).map(_.toLong).toList
  val distances = NumRe.findAllIn(lines(1)).map(_.toLong).toList
  val td        = times
    .zip(distances)
    .map: (time, dist) =>
      (1L until time).count(press => (time - press) * press > dist)
  td.product

def part2(lines: Vector[String]): Long =
  val time = NumRe.findAllIn(lines.head).mkString.toLong
  val dist = NumRe.findAllIn(lines(1)).mkString.toLong
  val t0   = ((time - Math.sqrt(time.toDouble * time - 4 * dist)) / 2).toLong
  val t1   = ((time + Math.sqrt(time.toDouble * time - 4 * dist)) / 2).toLong
  t1 - t0
