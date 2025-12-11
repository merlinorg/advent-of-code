package org.merlin.aoc
package year2025
package day11

import lib.{*, given}

@main def part1(): Unit =
  println(part1(sample1))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample2))
  println(part2(actual))

val sample1: String = load("sample1.txt")
val sample2: String = load("sample2.txt")
val actual: String  = load("actual.txt")

def part1(input: String): Long = countPaths("you", _ == "out", input.parse)

def part2(input: String): Long =
  val routes = input.parse.withDefaultValue(Set.empty)
  Vector("svr-dac-fft-out".split('-'), "svr-fft-dac-out".split('-')).sumMap: path =>
    path.slidingPairs.productMap((from, to) => countPaths(from, _ == to, routes))

extension (self: String)
  def parse: Map[String, Set[String]] = self.linesv.collectToMap:
    case s"$a: $b" => a -> b.split(" ").toSet
