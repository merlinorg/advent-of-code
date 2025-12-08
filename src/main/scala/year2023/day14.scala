package org.merlin.aoc
package year2023
package day14

import lib.{*, given}
import scala.annotation.tailrec

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: Vector[String] = loadv("sample.txt")
val actual: Vector[String] = loadv("actual.txt")

private type Dish = Vector[String]

extension (self: Dish)
  private def tilt: Dish =
    self.map(row => "#+|[^#]+".r.findAllIn(row).map(_.sorted.reverse).mkString)

  private def rotate: Dish =
    self.transpose.map(_.reverse.mkString)

  private def rotateᛌ: Dish =
    self.map(_.reverse).transpose.map(_.mkString)

  @tailrec private def cycle(n: Int, d: Dish = self, map: Map[Dish, Int] = Map.empty): Dish = map.get(d) match
    case Some(to) => map.map(_.swap)(to + n % (map.size - to))
    case None     => cycle(n - 1, d.tilt.rotate.tilt.rotate.tilt.rotate.tilt.rotate, map.updated(d, map.size))

  private def load: Long =
    self.transpose.zipWithIndex.sumMap((row, i) => row.count(_ == 'O') * (self.length - i))

def part1(dish: Dish): Long =
  dish.rotateᛌ.tilt.load

def part2(dish: Dish): Long =
  dish.rotateᛌ.cycle(1_000_000_000).load

// Part 2 in "7" lines

// def tilt(dish: Vector[String]): Vector[String] =
//   dish.map(row => "#+|[^#]+".r.findAllIn(row).map(_.sorted.reverse).mkString)
//
// @tailrec def loop(d: Vector[String], map: Map[Vector[String], Int]): Vector[String] = map.get(d) match
//   case Some(to) => map.map(_.swap)(to + (1000000000 - map.size) % (map.size - to))
//   case None     => loop((0 until 4).foldLeft(d)((d, _) => tilt(d).transpose.map(_.reverse.mkString)), map.updated(d, map.size))
//
// val dish = Source.fromResource("day-14.txt").getLines.toVector.map(_.reverse).transpose.map(_.mkString)
//
// loop(dish, Map.empty).transpose.zipWithIndex.foldMap((row, i) => row.count(_ == 'O') * (dish.size - i))
