package org.merlin.aoc
package year2023
package day08

import lib.impl.IO.{*, given}
import scala.annotation.tailrec

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(sample2))
  println(part2(actual))

val sample: Vector[String]  = loadv("sample.txt")
val sample2: Vector[String] = loadv("sample2.txt")
val actual: Vector[String]  = loadv("actual.txt")

private def parseRules(
  lines: Vector[String]
): Map[String, Map[Char, String]] =
  lines.foldLeft(Map.empty[String, Map[Char, String]]):
    case (acc, s"$from = ($l, $r)") =>
      acc.updated(from, Map('L' -> l, 'R' -> r))
    case (acc, _)                   => acc

def part1(lines: Vector[String]): Long =
  val lr    = LazyList.continually(lines.head).flatten
  val rules = parseRules(lines.drop(2))

  lr.scanLeft("AAA"): (pos, dir) =>
    rules(pos)(dir)
  .indexOf("ZZZ")

@tailrec private def gcd(x: Long, y: Long): Long =
  if y == 0 then x else gcd(y, x % y)

private def lcm(list: Iterable[Long]): Long =
  list.foldLeft(1L): (a, b) =>
    b * a / gcd(a, b)

def part2(lines: Vector[String]): Long =
  val lr    = LazyList.continually(lines.head).flatten
  val rules = parseRules(lines.drop(2))

  def count(start: String): Long =
    lr.scanLeft(start): (pos, dir) =>
      rules(pos)(dir)
    .indexWhere(_.endsWith("Z"))

  lcm(rules.keys.filter(_.endsWith("A")).map(count))
