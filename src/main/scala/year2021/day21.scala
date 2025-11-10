package org.merlin.aoc
package year2021.day21

import lib.{*, given}

import scala.collection.mutable

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
  val (p1, p2) = input.parse
  val iterator = Iterator.iterate((p1, 0, p2, 0, 0)): (p1, s1, p2, s2, d) =>
    val rnd =
      (d % 100) + (d + 1 % 100) + (d + 2 % 100) + 3
    if d % 2 == 0 then
      val p1p = (p1 + rnd - 1) % 10 + 1
      (p1p, s1 + p1p, p2, s2, d + 3)
    else
      val p2p = (p2 + rnd - 1) % 10 + 1
      (p1, s1, p2p, s2 + p2p, d + 3)
  iterator.findMap: (_, s1, _, s2, d) =>
    Option.when(s1 >= 1000 || s2 >= 1000):
      d * (if s1 >= 1000 then s2 else s1)

val Rolls = for
  i <- 1 to 3
  j <- 1 to 3
  k <- 1 to 3
yield i + j + k

def part2(input: String): Long =
  val cache = mutable.Map.empty[(Int, Int, Int, Int, Boolean), (Long, Long)]

  def solve(p1: Int, s1: Int, p2: Int, s2: Int, who: Boolean): (Long, Long) = cache.memo((p1, s1, p2, s2, who)):
    if s1 >= 21 then (1, 0)
    else if s2 >= 21 then (0, 1)
    else
      val scores =
        for roll <- Rolls
        yield
          val p = ((if who then p1 else p2) + roll - 1) % 10 + 1
          solve(
            if who then p else p1,
            if who then s1 + p else s1,
            if who then p2 else p,
            if who then s2 else s2 + p,
            !who
          )
      scores.foldLeft(0L -> 0L): (a, b) =>
        (a._1 + b._1, a._2 + b._2)

  val (p1, p2) = input.parse
  solve(p1, 0, p2, 0, true).fold(_ max _)

extension (self: String)
  def parse: (Int, Int) =
    self.linesv.map(_.replaceAll(".*: ", "").toInt).pairs.head
