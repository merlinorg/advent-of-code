package org.merlin.aoc
package year2021.day25

import lib.{*, given}

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

val sample: String = load("sample.txt")

val actual: String = load("actual.txt")

def part1(input: String): Long =
  Iterator
    .iteropt(input.linesv): grid =>
      val next = ">v".foldLeft(grid):
        case (base, cuke) =>
          base.gridIterator.foldLeft(base):
            case (grid, (loc, char)) =>
              val nxt = (loc + Dir(cuke)) %% grid
              if (char == cuke) && base.is(nxt, '.') then grid.updated(loc, '.').updated(nxt, cuke) else grid
      Option.when(next != grid)(next)
    .size
