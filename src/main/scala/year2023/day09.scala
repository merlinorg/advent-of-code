package org.merlin.aoc
package year2023
package day09

import lib.impl.IO.{*, given}
import lib.legacy.{*, given}
import scala.annotation.tailrec

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

@tailrec private def loop(v: List[Long], sum: Long): Long =
  v.zip(v.tail).map(_ - _) match
    case Nil   =>
      v.head + sum
    case diffs =>
      loop(diffs, v.head + sum)

def part1(lines: Vector[String]): Long =
  lines
    .map: line =>
      NumRe.findAllIn(line).map(_.toLong).toList.reverse
    .foldMap: values =>
      loop(values, 0)

def part2(lines: Vector[String]): Long =
  lines
    .map: line =>
      NumRe.findAllIn(line).map(_.toLong).toList
    .foldMap: values =>
      loop(values, 0)
