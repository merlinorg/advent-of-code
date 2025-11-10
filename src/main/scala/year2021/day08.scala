package org.merlin.aoc
package year2021.day08

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
    .sumMap: (_, output) =>
      output.count: str =>
        str.size < 5 || str.size > 6

def part2(input: String): Long =
  def permute(signals: Set[Char], permutation: String): Set[Char] =
    signals.map(char => permutation(char - 'a'))
  input.parse
    .sumMap: (input, output) =>
      val solution = "abcdefg".permutations.findFirst: permutation =>
        input.forall: signals =>
          digits.contains(permute(signals, permutation))
      output
        .map: signals =>
          digits.indexOf(permute(signals, solution))
        .mkString
        .toLong

val digits =
  Vector("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg").map(_.toSet)

extension (self: String)
  def parse: Vector[(Vector[Set[Char]], Vector[Set[Char]])] =
    self.linesv.collect:
      case s"$pre | $post" => pre.words.map(_.toSet) -> post.words.map(_.toSet)
