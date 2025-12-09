package org.merlin.aoc
package lib.impl

import IterableOps.*
import Vec2Ops.*
import lib.collection.*

object GridOps:
  extension (vector: Vector[String])
    def width: Int                           = vector.head.length
    def height: Int                          = vector.size
    def area: Int                            = width * height
    def dimensions: (Int, Int)               = (width, height)
    def southEast: Vec2                      = (width - 1, height - 1)
    def apply(xy: (Int, Int)): Char          = vector(xy._2)(xy._1)
    def get(xy: (Int, Int)): Option[Char]    = Option.when(xy >=< dimensions)(apply(xy))
    def is(xy: (Int, Int), c: Char): Boolean = (xy >=< dimensions) && (apply(xy) == c)
    def gridChars: Seq[Char]                 =
      for
        y <- 0 until height
        x <- 0 until width
      yield vector(y)(x)

    def gridIndices: Seq[(Int, Int)] =
      for
        y <- 0 until height
        x <- 0 until width
      yield x -> y

    def gridIterator: Seq[((Int, Int), Char)] =
      for
        y <- 0 until height
        x <- 0 until width
      yield ((x, y), vector(y)(x))

    def gridIndex(char: Char): (Int, Int) =
      gridIterator.findMap: (xy, c) =>
        Option.when(c == char)(xy)

    def gridIndices(char: Char): Set[(Int, Int)] =
      gridIterator.foldLeft(Set.empty[(Int, Int)]):
        case (set, (loc, `char`)) => set + loc
        case (set, _)             => set

    def updated(xy: Vec2, char: Char): Vector[String] =
      vector.updated(xy(1), vector(xy(1)).updated(xy(0), char))

    def has(xy: (Int, Int)): Boolean = xy >=< dimensions

    def stringify(f: ((Char, (Int, Int))) => Char): String =
      (0 until height).map(y => (0 until width).map(x => f((vector(y)(x), (x, y)))).mkString).mkString("\n")

    def gridMap(f: (Char, (Int, Int)) => Char): Vector[String] =
      vector.zipWithIndex.map: (s, y) =>
        s.zipWithIndex.map((c, x) => f(c, (x, y))).mkString

    def cw: Vector[String] =
      vector.transpose.map(_.mkString.reverse)

    def ccw: Vector[String] =
      vector.map(_.reverse).transpose.map(_.mkString)

    def flipX: Vector[String] =
      vector.map(_.reverse)

    def flipY: Vector[String] =
      vector.reverse

  extension (self: Iterable[Vec2])
    def toGrid: Vector[String] =
      self.strengthR('#').toMap.toGrid

  extension (self: Map[Vec2, Char])
    def toGrid: Vector[String] = if self.isEmpty then Vector.empty
    else
      val (minX, maxX) = self.keys.rangeMap(_._1)
      val (minY, maxY) = self.keys.rangeMap(_._2)
      self.foldLeft(Vector.fill(1 + maxY - minY)("." * (1 + maxX - minX))):
        case (grid, ((x, y), c)) => grid.updated((x - minX, y - minY), c)

  val EmptyMap: Map[Any, Any] = Map.empty
