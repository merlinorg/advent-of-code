package org.merlin.aoc
package year2024
package day20

import lib.impl.IO.{*, given}
import lib.legacy.{*, given}

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

def part1(maze: Vector[String]): Long = solve(maze, 2)

def part2(maze: Vector[String]): Long = solve(maze, 20)

def solve(maze: Vector[String], cheat: Long): Long =
  val start = maze.loc('S')
  val picos = if maze.length == 15 then 50 else 100

  val path = Vector((start, 0)) ++ Iterator.unfold((start, start, 1)): (prev, cur, steps) =>
    cur.adjacents
      .find(loc => !maze.is(loc, '#') && loc != prev)
      .map(loc => ((loc, steps), (cur, loc, steps + 1)))

  path.tails.sumCollect:
    case (loc0, dst0) +: tail =>
      tail
        .drop(picos)
        .count: (loc1, dst1) =>
          val dist = loc0.manhattan(loc1)
          (dist <= cheat) && (dst1 - dst0 - dist >= picos)
