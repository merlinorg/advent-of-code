package org.merlin.aoc
package year2020.day10

import lib.{*, given}

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
  val devices     = input.linesv.toLongs
  val differences = (0L +: devices.sorted :+ (devices.max + 3)).slidingPairs.map(_.difference).toBag
  differences(1) * differences(3)

def part2(input: String): Long =
  val devices = input.linesv.toLongs.toSet
  val end     = devices.max
  memoized[Long](0L): (value, loop) =>
    if value == end then 1
    else (value + 1 to value + 3).filter(devices).sumMap(loop)
