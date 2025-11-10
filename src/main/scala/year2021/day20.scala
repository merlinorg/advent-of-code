package org.merlin.aoc
package year2021.day20

import lib.{*, given}

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

def part1(input: String): Long = solve(input, 2)

def part2(input: String): Long = solve(input, 50)

def solve(input: String, iter: Int): Long =
  val (lookup, grid) = input.parse
  Iterator
    .iterate(("." * (2 + grid.width)) +: grid.map(s => s".$s.") :+ ("." * (2 + grid.width))): grid =>
      val result =
        (-1 to grid.height).map: y =>
          (-1 to grid.height).map: x =>
            val bits = (-1 to 1)
              .cross(-1 to 1)
              .map: dyx =>
                if grid.get((x, y) + dyx.swap).getOrElse(grid(Origin)) == '#' then '1' else '0'
            lookup(bits.mkString.parseBinary)
      result.toVector.map(_.mkString)
    .nth(iter)
    .sumMap(_.count(_ == '#'))

extension (self: String)
  def parse: (String, Vector[String]) =
    self.linesv.bichunk.lmap(_.head)
