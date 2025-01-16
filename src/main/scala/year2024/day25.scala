package org.merlin.aoc
package year2024
package day25

import lib.impl.IO.*
import scalaz.*
import Scalaz.*

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

val sample: Vector[String] = loadv("sample.txt")

val actual: Vector[String] = loadv("actual.txt")

def part1(lines: Vector[String]): Long =
  val (locks, keys) = parse(lines)
  locks.foldMap: lock =>
    keys.count: key =>
      key.locations.forall: loc =>
        key.is(loc, '.') || lock.is(loc, '.')

def parse(lines: Vector[String]): (Vector[Vector[String]], Vector[Vector[String]]) =
  val blocks = lines.selectSplit(_.nonEmpty).map(_.toVector).toVector
  blocks.partition(block => block.head.startsWith("#"))
