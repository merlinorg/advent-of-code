package org.merlin.aoc
package year2022.day02

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

def part1(input: String): Long =
  input.parse
    .sumCollect:
      case (Move(l), Move(r)) => r.value + (l <> r)

def part2(input: String): Long =
  input.parse
    .sumCollect:
      case (Move(l), x) =>
        val r = Move.fromOrdinal((l.ordinal + (if x == "X" then -1 else if x == "Y" then 0 else 1)) %% 3)
        r.value + (l <> r)

extension (string: String)
  def parse: Iterator[(String, String)] =
    string.linesIterator.map(_.tuple2)

enum Move:
  def value: Long          = this.ordinal + 1
  def <>(that: Move): Long =
    ((1 + that.ordinal - this.ordinal) %% 3) * 3

  case Rock, Paper, Scissors

object Move:
  val byName: Map[String, Move] = Map(
    "A" -> Rock,
    "B" -> Paper,
    "C" -> Scissors,
    "X" -> Rock,
    "Y" -> Paper,
    "Z" -> Scissors,
  )

  def unapply(s: String): Option[Move] = byName.get(s)
