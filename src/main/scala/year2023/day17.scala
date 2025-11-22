package org.merlin.aoc
package year2023
package day17

import lib.fp.{*, given}
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

val sample: Vector[String] = loadv("sample.txt")

val actual: Vector[String] = loadv("actual.txt")

private final case class Crucible(x: Int, y: Int, dx: Int, dy: Int, count: Int, loss: Int):
  def key: (Int, Int, Int, Int, Int) = (x, y, dx, dy, count)

  def forward: Crucible = copy(x = x + dx, y = y + dy, count = count + 1)
  def turn: Crucible    = copy(x = x + dy, y = y + dx, dx = dy, dy = dx, count = 1)
  def turnᛌ: Crucible   = copy(x = x - dy, y = y - dx, dx = -dy, dy = -dx, count = 1)

  def within(city: Vector[String]): Boolean = x >=< city.head.length && y >=< city.length

  def loseHeat(city: Vector[String]): Crucible = copy(loss = loss + within(city) ?? city(y)(x).asDigit)

private given Ordering[Crucible] = Ordering.by(c => -c.loss)

private def solve(city: Vector[String], min: Int, max: Int): Long =
  val seen  = mutable.Set.empty[(Int, Int, Int, Int, Int)] // ლ(ಠ益ಠლ)
  val queue = mutable.PriorityQueue(Crucible(0, 0, 1, 0, 0, 0), Crucible(0, 0, 0, 1, 0, 0))
  while queue.head.x != city.head.length - 1 || queue.head.y != city.length - 1 || queue.head.count < min do
    val crucible = queue.dequeue()
    if crucible.within(city) && seen.add(crucible.key) then
      if crucible.count < max then queue.enqueue(crucible.forward.loseHeat(city))
      if crucible.count >= min then queue.enqueue(crucible.turn.loseHeat(city), crucible.turnᛌ.loseHeat(city))
  queue.head.loss

def part1(city: Vector[String]): Long = solve(city, 0, 3)

def part2(city: Vector[String]): Long = solve(city, 4, 10)
