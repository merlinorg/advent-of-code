package org.merlin.aoc
package year2023
package day01

import lib.io.{*, given}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample2))
  println(part2(actual))

val sample: Vector[String]  = loadv("sample.txt")
val sample2: Vector[String] = loadv("sample2.txt")
val actual: Vector[String]  = loadv("actual.txt")

private val Digits = List(
  "zero",
  "one",
  "two",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine"
)

private val DigRe = ("[0-9]" :: Digits).mkString("|").r

def part1(lines: Vector[String]): Long =
  val calibrations = for
    line  <- lines
    first <- line.find(_.isDigit)
    last  <- line.findLast(_.isDigit)
  yield s"$first$last".toInt

  calibrations.sum

def part2(lines: Vector[String]): Long =
  val calibrations = for
    line      <- lines
    matches    = DigRe.findAllIn(line).toList
    first     <- matches.headOption
    firstIndex = Digits.indexOf(first)
    firstDigit = if firstIndex < 0 then first.toInt else firstIndex
    last      <- matches.lastOption
    lastIndex  = Digits.indexOf(last)
    lastDigit  = if lastIndex < 0 then last.toInt else lastIndex
  yield s"$firstDigit$lastDigit".toInt

  calibrations.sum
