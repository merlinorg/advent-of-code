package org.merlin.aoc
package year2024
package day14

import lib.impl.IO.*
import scalaz.*
import Scalaz.*

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: Vector[String] = loadv("sample.txt")

val actual: Vector[String] = loadv("actual.txt")

def part1(lines: Vector[String]): Long =
  given dim: XY = bathroom(lines)
  val robots    = Iterator
    .iterate(parse(lines))(_.map(_.move))
    .nth(100)
  (robots.groupBy(r => quadrant(r.pos)) - None).values.map(_.size).product

def part2(lines: Vector[String]): Long =
  given dim: XY = bathroom(lines)
  Iterator
    .iterate(parse(lines))(_.map(_.move))
    .takeWhile(probable)
    .size

// It is improbable that there will be ten robots above each other
private def probable(robots: Vector[Bot])(using dim: XY): Boolean =
  robots.groupBy(_.pos._1).values.forall: robots =>
    robots.map(_.pos._2).sorted.groupWhen(_ - _ == -1).forall(_.length < dim._2 / 10)

def parse(lines: Vector[String]): Vector[Bot] =
  lines.collect:
    case s"p=${L(x)},${L(y)} v=${L(vx)},${L(vy)}" =>
      Bot((x, y), (vx, vy))

def bathroom(lines: Vector[String]): XY =
  if lines.size == 12 then (11L, 7L) else (101L, 103L)

case class Bot(pos: XY, vel: XY):
  def move(using dim: XY): Bot = copy(pos = (pos + vel) % dim)

def quadrant(pos: XY)(using dim: XY): Option[Long] =
  Option.when(pos._1 != dim._1 / 2 && pos._2 != dim._2 / 2):
    2 * pos._1 / dim._1 + 2 * (2 * pos._2 / dim._2)

type XY = (Long, Long)

extension (self: XY)
  def +(xy: XY): XY = (self._1 + xy._1, self._2 + xy._2)
  def %(xy: XY): XY = (self._1 %% xy._1, self._2 %% xy._2)
