package org.merlin.aoc
package year2019
package day19

import lib.{*, given}

@main
def part1(): Unit =
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(actual))

val actual: String = load("actual.txt")

def part1(input: String): Long =
  (0 until 50).cross(0 until 50).count((x, y) => test(input, x, y))

def part2(input: String): Long =
  Iterator
    .iterate(0 -> 100): (x, y) =>
      Iterator.iterate(x)(_ + 1).findFirst(test(input, _, y + 1)) -> (y + 1)
    .findMap: (x, y) =>
      Option.when(test(input, x, y - 99) && test(input, x + 99, y - 99)):
        x * 10000 + y - 99

def test(input: String, x: Int, y: Int): Boolean =
  Computer(input, Vector(x, y)).runIO.get._1 == 1
