package org.merlin.aoc
package year2021.day12

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
  val graph = input.parse
  countPaths(
    ("start", Set("start")),
    (loc, _) => loc == "end",
    (loc, visited) =>
      graph(loc)
        .filter: dst =>
          dst.head.isUpper || !visited(dst)
        .map: dst =>
          dst -> (visited + dst)
  )

def part2(input: String): Long =
  val graph = input.parse
  countPaths(
    ("start", Set("start"), false),
    (loc, _, _) => loc == "end",
    (loc, visited, repeat) =>
      graph(loc)
        .filter: dst =>
          dst.head.isUpper || !visited(dst) || (dst != "start" && !repeat)
        .map: dst =>
          if dst.head.isUpper || !visited(dst) then (dst, visited + dst, repeat)
          else (dst, visited, true)
  )

extension (self: String)
  def parse: Map[String, Vector[String]] =
    val graph = self.linesv.flatMap:
      case s"$src-$dst" => Seq((src, dst), (dst, src))
    graph.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
