package org.merlin.aoc
package year2021.day17

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
  val (xs, ys) = input.parse
  solutions(xs, ys).max

def part2(input: String): Long =
  val (xs, ys) = input.parse
  solutions(xs, ys).size

def solutions(xs: Range, ys: Range): Iterable[Int] =
  (1 to xs.max)
    .cross(ys.min to ys.min.abs)
    .flatMap: velocity =>
      val (position, _) = Iterator
        .iterate(Origin -> velocity): (pos, vel) =>
          (pos + vel) -> (vel.x - vel.x.sign, vel.y - 1)
        .findFirst: (pos, _) =>
          (pos.y < ys.min) || (xs.contains(pos.x) && ys.contains(pos.y))
      Option.when(position.y >= ys.min)(velocity.y * (velocity.y + 1) / 2)

extension (self: String)
  def parse: (Range, Range) = self match
    case s"target area: x=${I(x0)}..${I(x1)}, y=${I(y0)}..${I(y1)}" => (x0 to x1) -> (y0 to y1)
