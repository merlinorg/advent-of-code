package org.merlin.aoc
package year2020.day07

import lib.{*, given}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long =
  val inverse = input.parse.toVector
    .flatMap: (outer, inners) =>
      inners.map: (inner, _) =>
        inner -> outer
    .toMultimap
  floodfill("shiny gold", inverse.getOrElse(_, Vector.empty)).size - 1

def part2(input: String): Long =
  val bags                    = input.parse
  def loop(src: String): Long =
    bags(src).sumMap: (dst, count) =>
      count * (1 + loop(dst))
  loop("shiny gold")

val LineRegex = "^(.*) bags contain (.*)\\.$".r
val BagRegex  = "(\\d+) (.*) bags?".r

extension (self: String)
  def parse: Map[String, Map[String, Int]] =
    self.linesv.collectToMap:
      case LineRegex(adjectives, "no other bags") =>
        adjectives -> Map.empty
      case LineRegex(adjectives, other)           =>
        adjectives -> other
          .split(", ")
          .collectToMap:
            case BagRegex(num, adjs) => adjs -> num.toInt
