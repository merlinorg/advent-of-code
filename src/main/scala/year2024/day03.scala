package org.merlin.aoc
package year2024
package day03

import lib.{*, given}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample2))
  println(part2(actual))

val sample: Vector[String]  = loadv("sample.txt")
val sample2: Vector[String] = loadv("sample2.txt")
val actual: Vector[String]  = loadv("actual.txt")

val Mul1Re = """mul\((\d+),(\d+)\)""".r

def part1(lines: Vector[String]): Long =
  lines.sumMap: line =>
    Mul1Re.findAllIn(line).toVector.sumMap(_.longs.product)

val Mul2Re = """mul\((\d+),(\d+)\)|do\(\)|don't\(\)""".r

def part2(lines: Vector[String]): Long =
  Mul2Re
    .findAllIn(lines.mkString)
    .toVector
    .foldLeft((0L, true)):
      case ((total, _), "do()")    => (total, true)
      case ((total, _), "don't()") => (total, false)
      case ((total, false), _)     => (total, false)
      case ((total, true), mul)    => (total + mul.longs.product, true)
    ._1