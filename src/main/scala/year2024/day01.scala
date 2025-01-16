package org.merlin.aoc
package year2024
package day01

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

def part1(lines: Vector[String]): Long =
  val (left, right) = lines.pairs.unzip
  left.sorted
    .zip(right.sorted)
    .foldMap(_ |-| _)

def part2(lines: Vector[String]): Long =
  val (left, right) = lines.pairs.unzip
  left.foldMap: digit =>
    digit * right.count(_ == digit) // quadratic shame

extension (self: Vector[String])
  def pairs: Vector[(Long, Long)] =
    self.map:
      case s"$a   $b" => (a.toLong, b.toLong)
