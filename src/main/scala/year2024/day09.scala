package org.merlin.aoc
package year2024
package day09

import lib.{*, given}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: Vector[String] = loadv("sample.txt")
val actual: Vector[String] = loadv("actual.txt")

def part1(lines: Vector[String]): Long =
  Iterator
    .unfold(parse(lines)):
      case (file +: files, blank +: blanks) if blank.pos < file.pos =>
        if blank.len == file.len then Some(file.inBlank(blank), (files, blanks))
        else if blank.len < file.len then Some(file.inBlank(blank), (file.dropRight(blank.len) +: files, blanks))
        else Some(file.inBlank(blank), (files, blank.dropLeft(file.len) +: blanks))
      case (file +: files, blanks)                                  =>
        Some(file, (files, blanks))
      case _                                                        => None
    .sumMap(_.value)

def part2(lines: Vector[String]): Long =
  Iterator
    .unfold(parse(lines)):
      case (file +: files, blanks) =>
        blanks.span(blank => blank.pos >= file.pos || blank.len < file.len) match
          case (pre, blank +: post) =>
            val insert = if file.len == blank.len then Vector.empty else Vector(blank.dropLeft(file.len))
            Some(file.inBlank(blank), (files, pre ++ insert ++ post))
          case _                    =>
            Some(file, (files, blanks))
      case _                       =>
        None
    .sumMap(_.value)

private def parse(lines: Vector[String]): (Vector[Extent], Vector[Extent]) =
  lines.head.toVector
    .foldLeft((true, 0, 0, (Vector.empty[Extent], Vector.empty[Extent]))):
      case ((true, index, pos, (files, blanks)), char)  =>
        (false, index, pos + char.asDigit, (Extent(index, pos, char.asDigit) +: files, blanks))
      case ((false, index, pos, (files, blanks)), char) =>
        (true, index + 1, pos + char.asDigit, (files, blanks :+ Extent(index, pos, char.asDigit)))
    ._4

private final case class Extent(id: Long, pos: Int, len: Int):
  def value: Long                     = id * (pos * len + len * (len - 1) / 2)
  def inBlank(extent: Extent): Extent = copy(pos = extent.pos, len = len min extent.len)
  def dropLeft(n: Int): Extent        = copy(pos = pos + n, len = len - n)
  def dropRight(n: Int): Extent       = copy(len = len - n)
