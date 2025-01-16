package org.merlin.aoc
package year2023
package day12

import lib.impl.IO.*
import scalaz.*
import Scalaz.*
import scala.collection.mutable

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

private def loop(
  springs: String,
  counts: List[Int],
  memo: mutable.Map[(String, Int), Long] = mutable.Map.empty
): Long =
  memo.getOrElseUpdate(
    springs -> counts.length,
    counts match
      case Nil if strMatch(springs, '.', springs.length) => 1L
      case Nil                                           => 0L
      case head :: tail                                  =>
        (1 to springs.length - head - tail.sum - tail.length).toList
          .filter: i =>
            strMatch(springs, '.', i) && strMatch(springs.substring(i), '#', head)
          .foldMap: i =>
            loop(springs.substring(i + head), tail, memo)
  )

private def strMatch(s: String, c: Char, n: Int): Boolean =
  s.substring(0, n).forall(d => d == c || d == '?')

def part1(lines: Vector[String]): Long =
  lines.foldMap:
    case s"$springs $counts" =>
      val springs1 = "." + springs
      val counts1  = counts.split(',').map(_.toInt).toList
      loop(springs1, counts1)

def part2(lines: Vector[String]): Long =
  lines.foldMap:
    case s"$springs $counts" =>
      val springs5 = "." + Array.fill(5)(springs).mkString("?")
      val counts5  = Array.fill(5)(counts.split(',').map(_.toInt)).flatten.toList
      loop(springs5, counts5)
