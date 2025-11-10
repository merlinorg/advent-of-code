package org.merlin.aoc
package year2021.day22

import lib.{*, given}

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

@main
def part1(): Unit =
  println(part1(sample1))
  println(part1(actual))

@main
def part2(): Unit =
  // perhaps an interval tree would improve performance..
  println(part2(sample2))
  println(part2(actual))

val sample1: String = load("sample1.txt")
val sample2: String = load("sample2.txt")

val actual: String = load("actual.txt")

def part1(input: String): Long =
  input.parse
    .foldLeft(Set.empty[Vec3]):
      case (cuboids, (on, (xs, ys, zs))) =>
        val cuboid = for
          x <- (xs(0) to xs(1)).intersect(-50 to 50)
          y <- (ys(0) to ys(1)).intersect(-50 to 50)
          z <- (zs(0) to zs(1)).intersect(-50 to 50)
        yield (x, y, z)
        if on then cuboids ++ cuboid else cuboids -- cuboid
    .size

def part2(input: String): Long =
  input.parse.zipWithIndex
    .foldLeft(Set.empty[Cuboid]):
      case (cuboids, ((on, cuboid), idx)) =>
        println(s".. $idx: ${cuboids.size}")
        @tailrec def loop(left: Seq[Cuboid], right: Seq[Cuboid]): Pair[Seq[Cuboid]] =
          val splitLeft  = right.foldLeft(left)(split)
          val splitRight = splitLeft.foldLeft(right)(split)
          if splitRight.size == right.size then splitLeft -> splitRight
          else loop(splitLeft, splitRight)

        val (intersecting, untouched)   = cuboids.partition(_.intersects(cuboid))
        val (splitCuboids, splitCuboid) = loop(intersecting.toSeq, Seq(cuboid))
        if on then untouched ++ splitCuboids ++ splitCuboid else untouched ++ splitCuboids -- splitCuboid
    .volume

def split(cs: Seq[Cuboid], d: Cuboid): Seq[Cuboid] =

  inline def splitAxis(c: Cuboid, axis: 0 | 1 | 2, value: Int): Seq[Cuboid] =
    if value <= c.get(axis)(0) || value > c.get(axis)(1) then Seq(c)
    else Seq(c.set(axis, c.get(axis)(0) -> (value - 1)), c.set(axis, value -> c.get(axis)(1)))

  def splitCuboid(c: Cuboid, d: Cuboid): Seq[Cuboid] =
    for
      c <- splitAxis(c, 0, d(0)(0))
      c <- splitAxis(c, 0, d(0)(1) + 1)
      c <- splitAxis(c, 1, d(1)(0))
      c <- splitAxis(c, 1, d(1)(1) + 1)
      c <- splitAxis(c, 2, d(2)(0))
      c <- splitAxis(c, 2, d(2)(1) + 1)
    yield c

  cs.par.flatMap(splitCuboid(_, d)).to(Seq)

type Cuboid = (Vec2, Vec2, Vec2)

extension (cuboid: Cuboid)
  def volume: Long =
    (0 to 2).productMap: axis =>
      cuboid.get(axis)(1).toLong - cuboid.get(axis)(0) + 1

  def intersects(other: Cuboid): Boolean =
    cuboid(0)(0) <= other(0)(1) && cuboid(0)(1) >= other(0)(0) &&
      cuboid(1)(0) <= other(1)(1) && cuboid(1)(1) >= other(1)(0) &&
      cuboid(2)(0) <= other(2)(1) && cuboid(2)(1) >= other(2)(0)

extension (cuboids: Iterable[Cuboid]) def volume: Long = cuboids.sumMap(_.volume)

extension (self: String)
  def parse: Vector[(Boolean, Cuboid)] =
    self.linesv.collect:
      case s"$bool x=${I(x0)}..${I(x1)},y=${I(y0)}..${I(y1)},z=${I(z0)}..${I(z1)}" =>
        (bool == "on", (x0 -> x1, y0 -> y1, z0 -> z1))
