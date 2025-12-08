package org.merlin.aoc
package year2020.day16

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
  val (rules, _, nearby) = input.parse
  nearby.flatten
    .filter: value =>
      rules.values.flatten.fornone(_.contains(value))
    .sum

def part2(input: String): Long =
  val (rules, mine, nearby) = input.parse
  val valid                 = nearby.filter: ticket =>
    ticket.forall: value =>
      rules.values.flatten.exists(_.contains(value))
  val result                = Iterator
    .iterate(rules -> Map.empty[Int, String]): (rules, result) =>
      val (name, index) = rules.toVector.findMap: (name, ranges) =>
        val possibilities = mine.indices.filter: index =>
          !result.contains(index) && valid.forall: ticket =>
            ranges.exists: range =>
              range.contains(ticket(index))
        Option.when(possibilities.length == 1)(name -> possibilities.head)
      (rules - name) -> (result + (index -> name))
    .findMap: (rules, result) =>
      Option.when(rules.isEmpty)(result)
  mine.zipWithIndex
    .flatMap: (value, index) =>
      Option.when(result(index).startsWith("departure"))(value.toLong)
    .product

extension (self: String)
  def parse: (Map[String, Vector[Range]], Vector[Int], Vector[Vector[Int]]) =
    val parts = self.linesv.chunks
    (
      parts(0).collectToMap:
        case s"$prop: ${I(f0)}-${I(t0)} or ${I(f1)}-${I(t1)}" =>
          prop -> Vector(f0 to t0, f1 to t1)
      ,
      parts(1)(1).integers,
      parts(2).drop(1).map(_.integers)
    )
