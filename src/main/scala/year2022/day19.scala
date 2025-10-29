package org.merlin.aoc
package year2022.day19

import lib.{*, given}

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
  input.parse.zipWithIndex.sumMap: (blueprint, index) =>
    (index + 1) * geodes(blueprint, 24)

def part2(input: String): Long =
  input.parse
    .take(3)
    .productMap: blueprint =>
      geodes(blueprint, 32)

def geodes(blueprint: Blueprint, minutes: Int): Long =
  val cache                                               = mutable.Map.empty[(Int, Vec3, Vec3), Int]
  // Returns how much geode production can be added
  def loop(time: Int, robots: Vec3, resources: Vec3): Int = cache.memo((time, robots, resources)):
    if time == 1 then 0
    else if resources >= blueprint(3) then // best to build geode robot
      time - 1 + loop(time - 1, robots, resources - blueprint(3) + robots)
    else if resources >= blueprint(2) then // next best to build obsidian robot
      loop(time - 1, robots.increment(2), resources - blueprint(2) + robots)
    else
      Vector(
        if resources >= blueprint(1) then loop(time - 1, robots.increment(1), resources - blueprint(1) + robots) else 0,
        if resources >= blueprint(0) then loop(time - 1, robots.increment(0), resources - blueprint(0) + robots) else 0,
        loop(time - 1, robots, resources + robots)
      ).max
  loop(minutes, (1, 0, 0), (0, 0, 0))

extension (self: Vec3)
  def increment(index: Int): Vec3 = index match
    case 0 => (self._1 + 1, self._2, self._3)
    case 1 => (self._1, self._2 + 1, self._3)
    case _ => (self._1, self._2, self._3 + 1)

  def >=(other: Vec3): Boolean =
    self._1 >= other._1 && self._2 >= other._2 && self._3 >= other._3

type Blueprint = (Vec3, Vec3, Vec3, Vec3)

extension (string: String)
  def parse: Vector[Blueprint] = string.linesv.collect:
    case s"Blueprint $_: Each ore robot costs ${I(oreOre)} ore. Each clay robot costs ${I(clayOre)} ore. Each obsidian robot costs ${I(
            obsidianOre
          )} ore and ${I(obsidianClay)} clay. Each geode robot costs ${I(geodeOre)} ore and ${I(geodeObsidian)} obsidian." =>
      ((oreOre, 0, 0), (clayOre, 0, 0), (obsidianOre, obsidianClay, 0), (geodeOre, 0, geodeObsidian))
