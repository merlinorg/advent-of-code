package org.merlin.aoc
package year2019
package day05

import lib.{*, given}

@main
def part1(): Unit =
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(actual))

val actual: String = load("actual.txt")

def part1(input: String): Long =
  Computer(input, Vector(1)).run.last().output.last

def part2(input: String): Long =
  Computer(input, Vector(5)).run.last().output.last
