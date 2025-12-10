package org.merlin.aoc
package year2021.day02

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
  input.parse.foldLeft(Origin)(_ + _).product

def part2(input: String): Long =
  input.parse
    .foldLeft((0, 0, 0)):
      case ((x, y, aim), dir) => (x + dir.x, y + aim * dir.x, aim + dir.y)
    .take(2)
    .product

extension (string: String)
  def parse: Vector[Vec2] = string.linesv.collect:
    case s"forward ${I(amount)}" => East * amount
    case s"down ${I(amount)}"    => South * amount
    case s"up ${I(amount)}"      => North * amount
