package org.merlin.aoc
package year2024
package day21

import lib.impl.IO.{*, given}
import lib.legacy.{*, given}
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

def part1(lines: Vector[String]): Long = solve(lines, 2)

def part2(lines: Vector[String]): Long = solve(lines, 25)

def solve(lines: Vector[String], robots: Int): Long =
  val cache = mutable.Map.empty[(Loc, Loc, Int), Long]

  def shortestMove(src: Loc, dst: Loc, stage: Int): Long = cache.memo((src, dst, stage)):
    given Monoid[Long]:
      def zero: Long                        = Long.MaxValue
      def combine(a0: Long, a1: Long): Long = a0.min(a1)
    val pad = if stage == 0 then Keypad else Dirpad
    bfsFoldl((src, Vector.empty[Char])): (loc, keys) =>
      (loc == dst).either(
        if stage < robots then shortedSolution(keys :+ 'A', stage + 1) else keys.length + 1L,
        (loc +-> dst).filterNot(dir => pad.is(loc + dir, ' ')).map(dir => (loc + dir, keys :+ DirKeys(dir)))
      )

  def shortedSolution(sequence: Vector[Char], stage: Int): Long =
    val pad = if stage == 0 then Keypad else Dirpad
    ('A' +: sequence)
      .map(pad.loc)
      .slidingPairs
      .foldMap:
        case (src, dst) => shortestMove(src, dst, stage)

  lines.foldMap: line =>
    shortedSolution(line.toVector, 0) * line.init.toLong

private val Keypad  = Vector("789", "456", "123", " 0A")
private val Dirpad  = Vector(" ^A", "<v>")
private val DirKeys = Map(Dir.E -> '>', Dir.W -> '<', Dir.N -> '^', Dir.S -> 'v')
