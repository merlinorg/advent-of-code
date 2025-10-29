package org.merlin.aoc
package year2022.day25

import lib.{*, given}

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

val sample: String = load("sample.txt")

val actual: String = load("actual.txt")

def part1(input: String): String =
  input.linesv.sumMap(_.desnafu).snafu

extension (self: Long)
  def snafu: String =
    Iterator
      .unfold(self): value =>
        value /% 5 match
          case (0, 0)     => None
          case (div, 3)   => Some(('=', div + 1))
          case (div, 4)   => Some(('-', div + 1))
          case (div, mod) => Some((('0' + mod).toChar, div))
      .mkString
      .reverse

extension (self: String)
  def desnafu: Long =
    self.foldLeft(0L):
      case (acc, '2') => acc * 5 + 2
      case (acc, '1') => acc * 5 + 1
      case (acc, '-') => acc * 5 - 1
      case (acc, '=') => acc * 5 - 2
      case (acc, _)   => acc * 5
