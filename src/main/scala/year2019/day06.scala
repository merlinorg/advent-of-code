package org.merlin.aoc
package year2019.day06

import lib.{*, given}

@main def part1(): Unit =
  println(part1(sample1))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample2))
  println(part2(actual))

val sample1: String = load("sample1.txt")
val sample2: String = load("sample2.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long =
  val graph                                = input.parse.toMultimap
  def loop(from: String, depth: Int): Long =
    depth + graph.getOrElse(from, Vector.empty).sumMap(loop(_, 1 + depth))
  loop("COM", 0)

def part2(input: String): Long =
  val graph = input.parse
    .flatMap: (from, to) =>
      Vector(from -> to, to -> from)
    .toMultimap
  minimumDistances(graph)("YOU")("SAN") - 2

extension (self: String)
  def parse: Vector[(String, String)] =
    self.linesv.collect:
      case s"$from)$to" => from -> to
