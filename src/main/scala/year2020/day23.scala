package org.merlin.aoc
package year2020.day23

import lib.{*, given}

import scala.annotation.tailrec

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

def part1(input: String): Long =
  val result = solve(input.toVector.map(_.asDigit), 100)

  @tailrec def loop(acc: Int): Int =
    val next = result(if acc == 0 then 1 else acc % 10)
    if next == 1 then acc else loop(acc * 10 + next)
  loop(0)

def part2(input: String): Long =
  val result = solve(input.toVector.map(_.asDigit) ++ (10 to 1_000_000), 10_000_000)
  val next   = result(1)
  next.toLong * result(next)

def solve(digits: Vector[Int], iter: Int): Map[Int, Int] =
  Iterator
    .iterate((digits.head, digits.slidingPairs.toMap + (digits.last -> digits.head))): (current, ordering) =>
      val pick0 = ordering(current)
      val pick1 = ordering(pick0)
      val pick2 = ordering(pick1)
      val next  = ordering(pick2)

      @tailrec def loop(i: Int): Int =
        if i == 0 then loop(ordering.size)
        else if i == pick0 || i == pick1 || i == pick2 then loop(i - 1)
        else i

      val dst         = loop(current - 1)
      val newOrdering = ordering + (current -> next) + (dst -> pick0) + (pick2 -> ordering(dst))
      next -> newOrdering
    .nth(iter)
    ._2
