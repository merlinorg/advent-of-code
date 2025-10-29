package org.merlin.aoc
package year2023
package day02

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

private type Colours = Map[String, Int]

private def isPlausible(round: Colours): Boolean =
  round("red") <= 12 && round("green") <= 13 && round("blue") <= 14

private def parse(line: String): (Int, List[Colours]) = line match
  case s"Game $id: $results" =>
    val rounds =
      results
        .split("; ")
        .toList
        .map: round =>
          round
            .split(", ")
            .collect:
              case s"$count $colour" =>
                colour -> count.toInt
            .toMap
            .withDefaultValue(0)
    id.toInt -> rounds

def part1(lines: Vector[String]): Long =
  val possibilities = lines
    .map(parse)
    .collect:
      case (id, rounds) if rounds.forall(isPlausible) => id

  possibilities.sum

def part2(lines: Vector[String]): Long =
  given maxMonoid: Monoid[Int] = Monoid.instance(0, _.max(_))

  val powers = lines
    .map(parse)
    .map:
      case (id, rounds) =>
        val minima = rounds.suml
        minima("red") * minima("green") * minima("blue")

  powers.sum
