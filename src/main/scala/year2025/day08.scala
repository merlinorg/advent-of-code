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
  solve(junctions)
    .nth(
      if junctions.length == 20 then 10 else 1000,
      _.circuits.toVector.map(_.size).sorted.takeRight(3).product
    )

def part2(input: String): Long =
  val junctions = input.parse
  solve(junctions).findMap: state =>
    Option.when(state.circuits.headOption.exists(_.size == junctions.size)):
      state.pair.productMap(_.x)

def solve(junctions: Vector[Vec3]): Iterator[(pair: Set[Vec3], circuits: Set[Set[Vec3]])] =
  val sorted = junctions.allPairs.sortBy(_ <-> _).map(_.toSet)
  (Iterator.single(Set.empty[Vec3]) ++ sorted).zip:
    sorted.iterator.scanLeft(Set.empty[Set[Vec3]]): (circuits, pair) =>
      circuits.partition(pair.exists).swap.fold(_ + _.foldLeft(pair)(_ ++ _))

extension (self: String) def parse: Vector[Vec3] = self.linesv.flatMap(Vec3.unapply)
