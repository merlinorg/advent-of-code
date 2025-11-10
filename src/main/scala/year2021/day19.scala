package org.merlin.aoc
package year2021.day19

import lib.{*, given}

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

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

def part1(input: String): Long = solve(input.parse)._1.size

def part2(input: String): Long =
  val (_, shifts) = solve(input.parse)
  shifts.cross(shifts).maxMap(_ |-| _)

def solve(scanners: Vector[Vector[Vec3]]): (Vector[Vec3], Vector[Vec3]) =
  @tailrec def loop(
    zero: Vector[Vec3],
    rest: Vector[Vector[Vec3]],
    shifts: Vector[Vec3]
  ): (Vector[Vec3], Vector[Vec3]) = rest match
    case head +: tail =>
      val solutions = for
        rotated <- head.map(_.rotations).transpose.par
        shift   <- rotated.cross(zero).map(_ - _)
        shifted  = rotated.map(_ - shift)
        overlap  = shifted.intersect(zero)
        if overlap.size >= 12
      yield shifted -> shift

      solutions.headOption match
        case None               => loop(zero, tail :+ head, shifts)
        case Some((fit, shift)) =>
          println(s".. ${tail.size} to go")
          loop((zero ++ fit).distinct, tail, shifts :+ shift)
    case _            => zero -> shifts
  loop(scanners.head, scanners.tail, Vector(Origin3D))

extension (self: String)
  def parse: Vector[Vector[Vec3]] =
    self.linesv.chunks.map: lines =>
      lines.tail.collect:
        case s"${I(x)},${I(y)},${I(z)}" => (x, y, z)
