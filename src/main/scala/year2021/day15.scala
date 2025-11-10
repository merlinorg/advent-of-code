package org.merlin.aoc
package year2021.day15

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

def part1(input: String): Long = solve(input.linesv)

def part2(input: String): Long =
  val grid     = input.linesv
  val expanded = (0 to 4).flatMap(y => grid.map(line => (0 to 4).map(x => line.map(c => clamp(c + x + y))).mkString))
  solve(expanded.toVector)

private def clamp(d: Int): Char = if d <= '9' then d.toChar else (d - 9).toChar

type Entry = (Vec2, Int)

given Ordering[Entry] = Ordering.by(c => -c._2)

def solve(grid: Vector[String]): Long =
  val end   = grid.southEast
  val queue = mutable.PriorityQueue.from(Seq((Origin, 0)))
  val best  = mutable.Map.empty[Vec2, Int]
  while queue.head._1 != end do
    val (loc, total) = queue.dequeue()
    val nexts        = for
      neighbour <- loc.neighbours
      value     <- grid.get(neighbour)
      cost       = total + value.asDigit
      if best.get(neighbour).forall(_ > cost)
    yield (neighbour, cost)
    nexts.foreach(best.update)
    queue.enqueue(nexts*)
  queue.head._2
