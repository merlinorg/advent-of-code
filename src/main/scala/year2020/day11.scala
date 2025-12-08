package org.merlin.aoc
package year2020.day11

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
  Iterator
    .last(input.linesv): grid =>
      val next = grid.gridMap: (c, loc) =>
        val count = loc.allNeighbours.count(grid.is(_, '#'))
        if c == 'L' && count == 0 then '#'
        else if c == '#' && count >= 4 then 'L'
        else c
      next.some - grid
    .gridChars
    .countA('#')

def part2(input: String): Long =
  Iterator
    .last(input.linesv): grid =>
      val next = grid.gridMap: (c, loc) =>
        val count = AllDirections.count: dir =>
          val path = Iterator.iteropt(loc + dir): loc =>
            Option.when(grid.is(loc, '.'))(loc + dir)
          grid.is(path.last(), '#')
        if c == 'L' && count == 0 then '#'
        else if c == '#' && count >= 5 then 'L'
        else c
      next.some - grid
    .gridChars
    .countA('#')
