package org.merlin.aoc
package year2022.day15

import lib.{*, given}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long =
  val sensors       = input.parse
  val y             = if sensors.length == 14 then 10 else 2000000
  val ranges        = sensors.ranges(y)
  val xs            = ranges.flatMap(r => Vector(r.min, r.max)).sorted
  val (_, _, total) = xs.foldLeft((Int.MinValue, false, 0L)):
    case ((_, false, acc), x) => (x, true, acc)
    case ((x0, true, acc), x) => (x + 1, ranges.exists(_.contains(x + 1)), acc + x - x0 + 1)
  total - sensors.beacons.count(_.y == y)

def part2(input: String): Long =
  val sensors = input.parse
  val max     = if sensors.length == 14 then 20 else 4000000
  val loc     = (0 to max).findMap: y =>
    val ranges      = sensors.ranges(y)
    val xs          = ranges.flatMap(r => Vector(r.min, r.max)).sorted.distinct
    val (_, result) = xs.foldLeft((false, Option.empty[Vec2])):
      case ((false, acc), x) => (true, acc orElse (x - 1 >=< max).option(x - 1 -> y))
      case ((true, acc), x)  => (ranges.exists(_.contains(x + 1)), acc)
    result
  loc.x * 4000000L + loc.y

type Sensor = ((Int, Int), (Int, Int), Int)

extension (self: Vector[Sensor])
  def ranges(y: Int): Vector[Range] = self.flatMap:
    case (sensor, _, dst) =>
      val dx = dst - (sensor.y - y).abs
      (dx >= 0).option(sensor.x - dx to sensor.x + dx)
  def beacons: Set[Vec2]            = self.map(_._2).toSet

extension (string: String)
  def parse: Vector[Sensor] = string.linesv.collect:
    case s"Sensor at x=${I(x0)}, y=${I(y0)}: closest beacon is at x=${I(x1)}, y=${I(y1)}" =>
      ((x0, y0), (x1, y1), (x0, y0) |-| (x1, y1))
