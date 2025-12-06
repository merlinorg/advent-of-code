package org.merlin.aoc
package year2025
package day06

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
  input.linesv
    .map("""\S+""".r.findAllIn(_).toVector)
    .transpose
    .sumMap: problem =>
      val values = problem.init.map(_.toLong)
      if problem.last == "*" then values.product else values.sum

def part2(input: String): Long =
  input.linesv.transpose
    .selectSplit(_.exists(_ != ' '))
    .sumMap: problem =>
      val values = problem.map(_.init.mkString.trim.toLong)
      if problem(0).last == '*' then values.product else values.sum
