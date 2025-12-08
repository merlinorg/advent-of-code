package org.merlin.aoc
package year2023
package day25

import lib.{*, given}
import scala.annotation.tailrec

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

val sample: Vector[String] = loadv("sample.txt")
val actual: Vector[String] = loadv("actual.txt")

type Graph = Map[String, Vector[String]]

extension (self: Vector[String])
  private def parse: Graph =
    val edges = self.flatMap:
      case s"$lhs: $rhss" => rhss.split(" ").map(lhs -> _)
    (edges ++ edges.map(_.swap)).toMultimap

// want https://en.wikipedia.org/wiki/Stoer%E2%80%93Wagner_algorithm but this seems to work shrug
@tailrec private def partition(graph: Graph, vertices: Set[String]): Set[String] =
  if vertices.sumMap(graph(_).count(!vertices(_))) == 3 then vertices
  else partition(graph, vertices + (graph.keySet -- vertices).minBy(graph(_).count(!vertices(_))))

def part1(lines: Vector[String]): Long =
  val graph = lines.parse
  val size  = partition(graph, Set(graph.keySet.head)).size
  size * (graph.size - size)
