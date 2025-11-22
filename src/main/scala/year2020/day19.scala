package org.merlin.aoc
package year2020.day19

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
  val (rules, inputs) = input.parse
  inputs.count: input =>
    rules.endIndices(0, input, 0)(input.length)

def part2(input: String): Long =
  val (rules, inputs) = input.parse
  val modified        = rules ++ Overrides
  inputs.count: input =>
    modified.endIndices(0, input, 0)(input.length)

val Overrides = Map(
  8  -> Set(Vector(42), Vector(42, 8)),
  11 -> Set(Vector(42, 31), Vector(42, 11, 31))
)

type Rule = Char | Set[Vector[Int]]

type Rules = Map[Int, Rule]

extension (rules: Rules)
  def endIndices(rule: Int, s: String, i: Int): Set[Int] = rules(rule) match
    case chr: Char                 =>
      Option.when(s.get(i).contains(chr))(i + 1).toSet
    case options: Set[Vector[Int]] =>
      options.flatMap: indices =>
        indices.foldLeft(Set(i)): (positions, rule) =>
          positions.flatMap(rules.endIndices(rule, s, _))

extension (self: String)
  def parse: (Map[Int, Rule], Vector[String]) =
    self.linesv.bichunk.lmap: lines =>
      lines.collectToMap:
        case s"${I(i)}: \"$s\"" => i -> s(0)
        case s"${I(i)}: $s"     => i -> s.split('|').toSet.map(_.integers)
