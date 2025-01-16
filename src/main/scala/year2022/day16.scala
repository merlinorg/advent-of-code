package org.merlin.aoc
package year2022.day16

import lib.*

import scala.collection.mutable

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
  solve(input.parse, 30, Set.empty)

// They say that the optimal solution is to split the work evenly.
def part2(input: String): Long =
  val valves       = input.parse
  val keys         = valves.filter(_._2._1 > 0).keySet
  val combinations = keys.toVector.combinations(keys.size / 2).map(_.toSet)
  combinations.maxMap: as =>
    solve(valves, 26, as) + solve(valves, 26, keys -- as)

given Ordering[(String, Set[String], Int, Long)] = Ordering.by(_._3)

def solve(valves: Map[String, Valve], minutes: Int, exclude: Set[String]): Long =
  val worthless = valves.filter(_._2._1 == 0).keySet
  val queue     = mutable.PriorityQueue(("AA", exclude ++ worthless, minutes, 0L))
  val visited   = mutable.Map.empty[(String, Set[String]), Long]
  while queue.nonEmpty do
    val (loc, opened, time, total) = queue.dequeue()
    if visited.get(loc -> opened).forall(_ < total) then
      visited.update(loc -> opened, total)
      if opened.size < valves.size && time > 1 then
        val (flow, dests) = valves(loc)
        if !opened(loc) then queue.enqueue((loc, opened + loc, time - 1, total + flow * (time - 1)))
        dests.foreach: (dst, dist) =>
          if !opened(dst) then queue.enqueue((dst, opened, time - dist, total))
  visited.values.max

type Valve = (Int, Map[String, Int])

extension (string: String)
  def parse: Map[String, Valve] =
    val valves    = string.linesv.collect:
      case s"Valve ${name} has flow rate=${I(rate)}; tunnel$_ lead$_ to valve$_ ${dest}" =>
        (name, rate, dest.split(", ").toVector)
    val distances = minimumDistances(valves.mapToMap((name, _, dests) => name -> dests))
    valves.mapToMap: (name, flow, _) =>
      name -> (flow -> distances(name))
