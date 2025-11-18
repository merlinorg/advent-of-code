package org.merlin.aoc
package year2024
package day25alt

import lib.{*, given}

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

val sample: String = load("sample.txt")

val actual: String = load("actual.txt")

def part1(input: String): Int =
  val (locks, keys) = input.split("\n\n").partition(_.startsWith("#"))

  val matches = for
    lock <- locks
    key  <- keys
    if lock
      .zip(key)
      .forall: (lockChar, keyChar) =>
        lockChar != '#' || keyChar != '#'
  yield lock -> key

  matches.length
