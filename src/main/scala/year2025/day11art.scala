package org.merlin.aoc
package year2025
package day11art

import lib.io.{*, given}
import scala.collection.mutable

@main def part1(): Unit =
  println(part1(sample1))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample2))
  println(part2(actual))

val sample1: String = load("sample1.txt")
val sample2: String = load("sample2.txt")
val actual: String  = load("actual.txt")

def part1(input: String): Long = input.parse.countPaths("you", "out")

def part2(input: String): Long =
  val adjacency = input.parse.withDefaultValue(Nil)
  List("svr-dac-fft-out", "svr-fft-dac-out")
    .map: route =>
      route
        .split('-')
        .sliding(2)
        .map: pair =>
          adjacency.countPaths(pair(0), pair(1))
        .product
    .sum

type AdjacencyList = Map[String, List[String]]

extension (self: String)
  def parse: AdjacencyList = self.linesIterator
    .collect:
      case s"$a: $b" => a -> b.split(' ').toList
    .toMap

import scala.language.experimental.namedTypeArguments

extension (adjacency: AdjacencyList)
  def countPaths(from: String, to: String): Long =
    memoised[Result = Long](from): (loc, loop) =>
      if loc == to then 1L else adjacency(loc).map(loop).sum

def memoised[A, Result](init: A)(f: (A, A => Result) => Result): Result =
  class Memoise extends (A => Result):
    val memo                = mutable.Map.empty[A, Result]
    def apply(a: A): Result = memo.getOrElseUpdate(a, f(a, this))
  Memoise()(init)
