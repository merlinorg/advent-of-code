package org.merlin.aoc
package lib.impl

import lib.impl.IterableOps.*
import lib.impl.Vec2Ops.*

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
    def gridIndices: Seq[(Int, Int)]         =
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

    def updated(xy: Vec2, char: Char): Vector[String] =
      vector.updated(xy(1), vector(xy(1)).updated(xy(0), char))

    def contains(xy: (Int, Int)): Boolean = xy >=< dimensions

    def stringify(f: ((Char, (Int, Int))) => Char): String =
      (0 until height).map(y => (0 until width).map(x => f((vector(y)(x), (x, y)))).mkString).mkString("\n")

    def gridMap(f: (Char, (Int, Int)) => Char): Vector[String] =
      vector.zipWithIndex.map: (s, y) =>
        s.zipWithIndex.map((c, x) => f(c, (x, y))).mkString

  extension (self: Iterable[Vec2])
    def toGrid: Vector[String] = self.foldLeft(Vector.fill(1 + self.maxMap(_._2))("." * (1 + self.maxMap(_._1)))):
      (grid, point) => grid.updated(point, '#')
