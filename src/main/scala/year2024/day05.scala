package org.merlin.aoc
package year2024
package day05

import lib.{*, given}

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

def part1(lines: Vector[String]): Long =
  val (rules, updates) = parse(lines)
  updates.filter(valid(_, rules)).sumMap(_.middle)

def part2(lines: Vector[String]): Long =
  val (rules, updates) = parse(lines)
  updates
    .filterNot(valid(_, rules))
    .sumMap(_.sortWith((a, b) => rules.contains(a -> b)).middle)

private type Rules  = Vector[(Long, Long)]
private type Update = Vector[Long]

private def valid(update: Update, rules: Rules): Boolean =
  rules.forall: (a, b) =>
    !update.contains(a) || !update.contains(b) || update.indexOf(a) < update.indexOf(b)

private def parse(lines: Vector[String]): (Rules, Vector[Update]) =
  lines
    .span(_.nonEmpty)
    .bimap(
      _.map({ case s"$a|$b" => (a.toLong, b.toLong) }),
      _.tail.map(_.longs)
    )
