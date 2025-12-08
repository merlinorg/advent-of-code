package org.merlin.aoc
package year2022.day21

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
  input.parse.solve("root")

def part2(input: String): Long =
  val map                 = input.parse
  val (human, _, inhuman) = map.partial("root").get
  map.unsolve(human, inhuman)

extension (self: Graph)
  def solve(name: String): Long = self(name) match
    case Right(value)      => value
    case Left((a, '+', b)) => solve(a) + solve(b)
    case Left((a, '-', b)) => solve(a) - solve(b)
    case Left((a, '*', b)) => solve(a) * solve(b)
    case Left((a, _, b))   => solve(a) / solve(b)

  def unsolve(name: String, value: Long): Long = partial(name) match
    case Some((human, op, partial)) =>
      val remainder = op match
        case '+' => value - partial
        case '-' => value + partial
        case '!' => partial - value
        case '*' => value / partial
        case _   => value * partial
      unsolve(human, remainder)
    case None                       => value

  def partial(name: String): Option[(String, Char, Long)] = self(name).left.toOption.map:
    case (a, op, b) =>
      val (human, inhuman) = if contains(a, "humn") then (a, b) else (b, a)
      (human, if op == '-' && b == human then '!' else op, solve(inhuman))

  def contains(name: String, value: String): Boolean = name == value || (self(name) match
    case Right(_)        => false
    case Left((a, _, b)) => contains(a, value) || contains(b, value))

type Graph = Map[String, Either[(String, Char, String), Long]]

extension (string: String)
  def parse: Graph = string.linesv.collectToMap:
    case s"$name: $left ${C(op)} $right" => name -> Left((left, op, right))
    case s"$name: ${L(value)}"           => name -> Right(value)
