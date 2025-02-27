package org.merlin.aoc
package year2023
package day24

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

private final case class Hail(x: Long, y: Long, z: Long, vx: Long, vy: Long, vz: Long):
  def xyProjection: Hail2D = Hail2D(x, y, vx, vy)
  def xzProjection: Hail2D = Hail2D(x, z, vx, vz)

private final case class Hail2D(x: Long, y: Long, vx: Long, vy: Long):
  private val a: BigDecimal = BigDecimal(vy)
  private val b: BigDecimal = BigDecimal(-vx)
  private val c: BigDecimal = BigDecimal(vx * y - vy * x)

  def deltaV(dvx: Long, dvy: Long): Hail2D = copy(vx = vx - dvx, vy = vy - dvy)

  // If the paths of these hailstones intersect, return the location of the intersection
  def intersect(hail: Hail2D): Option[(BigDecimal, BigDecimal)] =
    val denominator = a * hail.b - hail.a * b
    Option.when(denominator != 0):
      ((b * hail.c - hail.b * c) / denominator, (c * hail.a - hail.c * a) / denominator)

  // Return the time at which this hail will intersect the given point on its path
  def timeTo(posX: BigDecimal, posY: BigDecimal): BigDecimal = if vx == 0 then (posY - y) / vy else (posX - x) / vx
end Hail2D

extension (self: Vector[String])
  private def parse: Vector[Hail] =
    self.map:
      case s"$x, $y, $z @ $dx, $dy, $dz" =>
        Hail(x.trim.toLong, y.trim.toLong, z.trim.toLong, dx.trim.toLong, dy.trim.toLong, dz.trim.toLong)

private def intersections(hails: Vector[Hail2D], min: Long, max: Long): Vector[(Hail2D, Hail2D)] =
  for
    (hail0, hail1) <- hails.allPairs
    (x, y)         <- hail0.intersect(hail1)
    if x >= min && x <= max && y >= min && y <= max && hail0.timeTo(x, y) >= 0 && hail1.timeTo(x, y) >= 0
  yield (hail0, hail1)

def part1(lines: Vector[String]): Long =
  val hailsXY = lines.parse.map(_.xyProjection)
  if lines.length == 5 then intersections(hailsXY, 7, 27).size
  else intersections(hailsXY, 200000000000000L, 400000000000000L).size

private def findOrigin(hails: Vector[Hail2D], dx: Long, dy: Long): Option[(Long, Long)] =
  val hail0 +: hail1 +: hail2 +: _ = hails.map(_.deltaV(dx, dy)): @unchecked
  for
    (x0, y0) <- hail0.intersect(hail1)
    (x1, y1) <- hail0.intersect(hail2)
    if x0 == x1 && y0 == y1
    time      = hail0.timeTo(x0, y0)
  yield (hail0.x + hail0.vx * time.longValue, hail0.y + hail0.vy * time.longValue)

private final case class Spiral(x: Long, y: Long, dx: Long, dy: Long, count: Long, limit: Long):
  def next: Spiral =
    if count > 0 then copy(x = x + dx, y = y + dy, count = count - 1)
    else if dy == 0 then copy(x = x + dx, y = y + dy, dy = dx, dx = -dy, count = limit)
    else copy(x = x + dx, y = y + dy, dy = dx, dx = -dy, count = limit + 1, limit = limit + 1)

private object Spiral:
  final val Start = Spiral(0, 0, 1, 0, 0, 0)

def part2(lines: Vector[String]): Long =
  val hails = lines.parse

  val hailsXY = hails.map(_.xyProjection)
  val (x, y)  = Iterator
    .iterate(Spiral.Start)(_.next)
    .findMap: spiral =>
      findOrigin(hailsXY, spiral.x, spiral.y)

  val hailsXZ = hails.map(_.xzProjection)
  val (_, z)  = Iterator
    .iterate(Spiral.Start)(_.next)
    .findMap: spiral =>
      findOrigin(hailsXZ, spiral.x, spiral.y)

  x + y + z
