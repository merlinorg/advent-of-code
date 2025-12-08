package org.merlin.aoc
package year2020.day04

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
  input.parse.count: pp =>
    Required.forall(pp.contains)

def part2(input: String): Long =
  input.parse.count: pp =>
    Required.forall(pp.contains) && pp.forall:
      case ("byr", value)         => value.length == 4 && value.toInt >= 1920 && value.toInt <= 2002
      case ("iyr", value)         => value.length == 4 && value.toInt >= 2010 && value.toInt <= 2020
      case ("eyr", value)         => value.length == 4 && value.toInt >= 2020 && value.toInt <= 2030
      case ("hgt", s"${I(cm)}cm") => cm >= 150 && cm <= 193
      case ("hgt", s"${I(in)}in") => in >= 59 && in <= 76
      case ("hcl", Hcl(_))        => true
      case ("ecl", Ecl(_))        => true
      case ("pid", Pid(_))        => true
      case ("cid", _)             => true
      case _                      => false

val Required = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

val Hcl = "#([0-9a-f]{6})".r
val Ecl = "(amb|blu|brn|gry|grn|hzl|oth)".r
val Pid = "([0-9]{9})".r

extension (self: String)
  def parse: Vector[Map[String, String]] =
    self.linesv.chunks.map: chunk =>
      chunk
        .mkString(" ")
        .split(' ')
        .collectToMap:
          case s"${attr}:${value}" => attr -> value
