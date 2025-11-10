package org.merlin.aoc
package year2021.day04

import lib.{*, given}

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

type Board = Vector[Vector[Int]]

def part1(input: String): Long =
  val (digits, boards) = input.parse
  Iterator
    .from(digits)
    .scanLeft(Set.empty[Int] -> Option.empty[Long]):
      case ((prior, _), number) =>
        val drawn = prior + number
        drawn -> boards.find(_.isComplete(drawn)).map(_.value(drawn, number))
    .findMap(_._2)

def part2(input: String): Long =
  val (digits, boards) = input.parse
  Iterator
    .from(digits)
    .scanLeft((Set.empty[Int], boards, Option.empty[Long])):
      case ((prior, boards, _), number) =>
        val drawn                  = prior + number
        val (complete, incomplete) = boards.partition(_.isComplete(drawn))
        (
          drawn,
          incomplete,
          complete.headOption.when(boards.size == 1).map(_.value(drawn, number))
        )
    .findMap(_._3)

extension (self: Board)
  def isComplete(drawn: Set[Int]): Boolean =
    self.indices.exists: i =>
      self.indices.forall(j => drawn(self(i)(j))) ||
        self.indices.forall(j => drawn(self(j)(i)))

  def value(drawn: Set[Int], last: Int): Long =
    self.flatten.filterNot(drawn).sum.toLong * last

extension (string: String)
  def parse: (Vector[Int], Vector[Board]) =
    val lines = string.linesv
    lines.head.integers -> lines.tail.tail.chunks.map(_.map(_.integers))
