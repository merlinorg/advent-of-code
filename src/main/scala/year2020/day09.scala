package org.merlin.aoc
package year2020.day09

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
  solve(input.linesv.toLongs)

def part2(input: String): Long =
  val numbers = input.linesv.toLongs
  val invalid = solve(input.linesv.toLongs)

  @tailrec def loop(start: Int, finish: Int, sum: Long): Long =
    if sum == invalid then
      val range = numbers.slice(start, finish)
      range.min + range.max
    else if sum > invalid then loop(start + 1, finish, sum - numbers(start))
    else loop(start, finish + 1, sum + numbers(finish))
  loop(0, 0, 0)

def solve(numbers: Vector[Long]): Long =
  val window = if numbers.length == 20 then 5 else 25
  numbers
    .sliding(1 + window)
    .findCollect:
      case window :+ next if window.allPairs.forall(t => t.sum != next) => next
