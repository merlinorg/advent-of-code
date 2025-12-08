package org.merlin.aoc
package year2025
package day08

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
  val junctions = input.parse
  val circuits  = solve(junctions)
    .nth(if junctions.length == 20 then 10 else 1000, _.circuits)
  circuits.values.map(_.size).toVector.sorted.reverse.take(3).product

def part2(input: String): Long =
  val junctions = input.parse
  solve(junctions).findMap: state =>
    Option.when(state.jids.size == junctions.size && state.circuits.size == 1):
      state.pair._1.x.toLong * state.pair._2.x

type CircuitId = Long

def solve(
  junctions: Vector[Vec3]
): Iterator[(jids: Map[Vec3, CircuitId], circuits: Map[CircuitId, Set[Vec3]], pair: (Vec3, Vec3), id: CircuitId)] =
  junctions.allPairs
    .sortBy((a, b) => a.distance2(b))
    .iterator
    .scanLeft(Map.empty[Vec3, CircuitId], Map.empty[CircuitId, Set[Vec3]], Origin3D -> Origin3D, 0L):
      case ((jids, circuits, _, id), (a, b)) =>
        (jids.get(a), jids.get(b)) match
          case (None, None)       =>
            (jids + (a -> id) + (b -> id), circuits + (id -> Set(a, b)), a -> b, id + 1)
          case (Some(c), None)    =>
            (jids + (b -> c), circuits + (c -> (circuits(c) + b)), a -> b, id)
          case (None, Some(d))    =>
            (jids + (a -> d), circuits + (d -> (circuits(d) + a)), a -> b, id)
          case (Some(c), Some(d)) =>
            (jids ++ circuits(d).strengthR(c), circuits - d + (c -> (circuits(c) ++ circuits(d))), a -> b, id)

extension (self: String)
  def parse: Vector[Vec3] = self.linesv.collect:
    case s"${I(x)},${I(y)},${I(z)}" => (x, y, z)
