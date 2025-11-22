package org.merlin.aoc
package year2020.day25

import lib.{*, given}

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

val sample: String = load("sample.txt")

val actual: String = load("actual.txt")

def part1(input: String): Long =
  val keys = input.longs
  transform(keys(0)).nth(transform(7).takeUntil(_ == keys(1)).size)

def transform(subject: Long): Iterator[Long] =
  Iterator.iterate(1L)(v => (v * subject) % 20201227)
