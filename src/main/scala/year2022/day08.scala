package org.merlin.aoc
package year2022.day08

import lib.{*, given}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long =
  val grid = input.linesv
  val res  = for
    ((x, y), c) <- grid.gridIterator
    if (0 until x).forall(xx => c > grid(xx -> y)) ||
      (0 until y).forall(yy => c > grid(x -> yy)) ||
      (grid.width - 1 until x by -1).forall(xx => c > grid(xx -> y)) ||
      (grid.height - 1 until y by -1).forall(yy => c > grid(x -> yy))
  yield ()
  res.length

def part2(input: String): Long =
  val grid = input.linesv
  val res  =
    for ((x, y), c) <- grid.gridIterator
    yield (x - 1 to 0 by -1).takeTo(xx => c > grid(xx -> y)).length *
      (x + 1 until grid.width).takeTo(xx => c > grid(xx -> y)).length *
      (y - 1 to 0 by -1).takeTo(yy => c > grid(x -> yy)).length *
      (y + 1 until grid.height).takeTo(yy => c > grid(x -> yy)).length
  res.max
