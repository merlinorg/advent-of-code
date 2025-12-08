package org.merlin.aoc
package year2024
package day16mut

import lib.io.{*, given}
import scala.collection.mutable

import day16alt.{Reindeer, findPosition, East, Position, Direction, Maze}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Int =
  val (reindeer, _) = solve(input)
  reindeer.score

def part2(input: String): Int =
  val (reindeer, queue) = solve(input)
  val paths             = mutable.Set.from(reindeer.path)
  while queue.head.score == reindeer.score do
    val next = queue.dequeue()
    if next.pos == reindeer.pos then paths.addAll(next.path)
  paths.size

def solve(input: String): (Reindeer, mutable.PriorityQueue[Reindeer]) =
  val maze     = input.split("\n")
  val start    = maze.findPosition('S').get
  val end      = maze.findPosition('E').get
  val reindeer = Reindeer(0, start, East, Vector(start))
  val visited  = mutable.Set.empty[(Position, Direction)]
  val queue    = mutable.PriorityQueue.from(Seq(reindeer))

  while queue.head.pos != end do
    val reindeer = queue.dequeue()

    val neighbours = reindeer.neighbours.filter: next =>
      maze(next.pos) != '#' && !visited(next.pos -> next.dir)

    visited.addOne(reindeer.pos -> reindeer.dir)
    queue.addAll(neighbours)

  (queue.dequeue(), queue)

given Ordering[Reindeer] = Ordering.by(-_.score)

extension (maze: Maze) def apply(position: Position): Char = maze(position(1))(position(0))
