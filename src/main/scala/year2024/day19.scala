package org.merlin.aoc
package year2024
package day19

import lib.{*, given}

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: Vector[String] = loadv("sample.txt")

val actual: Vector[String] = loadv("actual.txt")

def part1(lines: Vector[String]): Long =
  val (towels, patterns) = parse(lines)
  patterns.count(
    Y[String, Boolean](
      (self, pattern) =>
        pattern.isEmpty || towels.exists: towel =>
          pattern.startsWith(towel) && self(pattern.substring(towel.length)),
      _
    )
  )

def Y[A, B](f: (A => B, A) => B, x: A): B =
  f(v => Y(f, v), x)

def part2(lines: Vector[String]): Long =
  val (allTowels, patterns) = parse(lines)
  Iterator
    .iterate((Vector(patterns.head -> allTowels), Map("" -> 1L), patterns, 0L)):
      case ((pattern, _) +: tail, cache, patterns, total) if cache.contains(pattern)                 =>
        (tail, cache, patterns, total)
      case ((pattern, towel +: towels) +: tail, cache, patterns, total) if pattern.startsWith(towel) =>
        ((pattern.substring(towel.length), allTowels) +: (pattern, towels) +: tail, cache, patterns, total)
      case ((pattern, _ +: towels) +: tail, cache, patterns, total)                                  =>
        ((pattern, towels) +: tail, cache, patterns, total)
      case ((pattern, _) +: tail, cache, patterns, total)                                            =>
        val subtotal = allTowels.sumMap: towel =>
          if pattern.startsWith(towel) then cache(pattern.substring(towel.length)) else 0
        (tail, cache + (pattern -> subtotal), patterns, total)
      case (_, cache, pattern +: next +: patterns, total)                                            =>
        (Vector(next -> allTowels), cache, next +: patterns, total + cache(pattern))
      case (_, cache, pattern +: patterns, total)                                                    =>
        (Vector.empty, cache, patterns, total + cache(pattern))
      case done                                                                                      => done
    .findMap:
      case (_, _, patterns, total) => Option.when(patterns.isEmpty)(total)

private def parse(lines: Vector[String]): (Vector[String], Vector[String]) =
  (lines(0).split(", ").toVector, lines.drop(2))
