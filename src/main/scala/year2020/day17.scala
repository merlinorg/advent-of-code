package org.merlin.aoc
package year2020.day17

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
  solve:
    input.linesv.gridIterator.collectToSet:
      case ((x, y), '#') => (x, y, 0)

def part2(input: String): Long =
  solve:
    input.linesv.gridIterator.collectToSet:
      case ((x, y), '#') => (x, y, 0, 0)

def solve[A: Neighbourly as N](state: Set[A]): Long =
  Iterator
    .iterate(state): state =>
      def count(loc: A): Int = N.allNeighbours(loc).count(state)
      state.flatMap: loc =>
        N.allNeighbours(loc)
          .filter: neighbour =>
            !state(neighbour) && count(neighbour) == 3
          .concat:
            val ct = count(loc)
            Option.when(ct == 2 || ct == 3)(loc)
    .nth(6)
    .size

trait Neighbourly[A]:
  def allNeighbours(a: A): Iterable[A]

given Neighbourly[(Int, Int, Int)] = loc => N3.map(n => (n(0) + loc(0), n(1) + loc(1), n(2) + loc(2)))

given Neighbourly[(Int, Int, Int, Int)] = loc =>
  N4.map(n => (n(0) + loc(0), n(1) + loc(1), n(2) + loc(2), n(3) + loc(3)))

val R  = -1 to 1
val N3 = R.flatMap(x => R.flatMap(y => R.flatMap(z => Option.when(x != 0 || y != 0 || z != 0)((x, y, z)))))
val N4 = R.flatMap: w =>
  R.flatMap(x => R.flatMap(y => R.flatMap(z => Option.when(w != 0 || x != 0 || y != 0 || z != 0)((w, x, y, z)))))
