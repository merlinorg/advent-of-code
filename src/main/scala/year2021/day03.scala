package org.merlin.aoc
package year2021.day03

import lib.{*, given}

import scala.annotation.tailrec

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
  val parsed                       = input.linesv
  def rate(most: Boolean): Long =
    def digit(pos: Int): Char =
      if (parsed.count(_(pos) == '1') > parsed.length / 2) == most then '1' else '0'
    parsed.head.indices.toVector.map(digit).mkString.parseBinary
  Vector(true, false).map(rate).product

def digit(parsed: Vector[String], pos: Int, most: Boolean): Char =
  if (parsed.count(_(pos) == '1') >= (parsed.length +1 / 2)) == most then '1' else '0'

def part2(input: String): Long =
  @tailrec def loop(input: Vector[String], pos: Int, most: Boolean): Long =
    if input.length == 1 then input.head.parseBinary
    else
      val chr = if (input.count(_(pos) == '1') >= (input.length + 1) / 2) == most then '1' else '0'
      loop(input.filter(s => s(pos) == chr), pos + 1, most)

  val parsed = input.linesv
  Vector(true, false).map(loop(parsed, 0, _)).product
