package org.merlin.aoc
package year2019
package day16

import lib.{*, given}

@main
def part1(): Unit =
  println(part1(sample1))
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(sample2))
  println(part2(actual))

val sample1: String = load("sample1.txt")
val sample2: String = load("sample2.txt")

val actual: String = load("actual.txt")

def part1(input: String): Long =
  Iterator
    .iterate(input.toVector.map(_.asDigit)): digits =>
      digits.indices.toVector.map: index =>
        pattern(index).zip(digits).sumMap(_ * _).abs % 10
    .nth(100, _.take(8).foldLeft(0L)(_ * 10 + _))

// The offset drops the first half of the list and the second half
// of the list follows a trivial pattern
def part2(input: String): Long =
  val digits = input.toVector.map(_.asDigit) * 10_000
  val offset = digits.take(7).foldLeft(0)(_ * 10 + _)
  Iterator
    .iterate(digits.drop(offset)): digits =>
      digits.scanRight(0)(_ + _).dropRight(1).map(_ % 10)
    .nth(100, _.take(8).foldLeft(0)(_ * 10 + _))

def pattern(i: Int): Iterator[Int] =
  Iterator.repeat(Seq(0, 1, 0, -1)).flatMap(Iterator.fill(i + 1)).drop(1)
