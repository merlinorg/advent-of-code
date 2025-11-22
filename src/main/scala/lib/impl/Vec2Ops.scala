package org.merlin.aoc
package lib.impl

import lib.impl.BooleanOps.*
import lib.impl.GridOps.*
import lib.impl.IntOps.*
import lib.impl.IteratorOps.*

object Vec2Ops:
  type Vec2 = (Int, Int)

  val North = (0, -1)
  val South = (0, 1)
  val East  = (1, 0)
  val West  = (-1, 0)

  val NorthEast = (1, -1)
  val SouthEast = (1, 1)
  val NorthWest = (-1, -1)
  val SouthWest = (-1, 1)

  val Dir = Map(
    "NU^" -> North,
    "SDv" -> South,
    "ER>" -> East,
    "WL<" -> West
  ).flatMap((str, dir) => str.map(_ -> dir))

  val CardinalDirections = Vector(North, East, South, West)
  val AllDirections      = Vector(North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest)

  val Origin = (0, 0)

  val Nowhere = (Int.MaxValue, Int.MaxValue)

  private val cwMap  = AllDirections.indices.map(i => AllDirections(i) -> AllDirections((i + 1) % 8)).toMap
  private val ccwMap = cwMap.map((a, b) => b -> a)

  extension (vec: Vec2)
    def x: Int               = vec(0)
    def y: Int               = vec(1)
    def abs: Vec2            = (x.abs, y.abs)
    def sign: Vec2           = (x.sign, y.sign)
    def *(scalar: Int): Vec2 = (x * scalar, y * scalar)

    def +(other: Vec2): Vec2           = append(other, _ + _)
    def %(other: Vec2): Vec2           = append(other, _ % _)
    def %(grid: Vector[String]): Vec2  = append(grid.dimensions, _ % _)
    def %%(other: Vec2): Vec2          = append(other, posMod)
    def %%(grid: Vector[String]): Vec2 = append(grid.dimensions, posMod)
    def -(other: Vec2): Vec2           = append(other, _ - _)
    def >=<(other: Vec2): Boolean      = x >= 0 && x < other.x && y >= 0 && y < other.y
    def |-|(other: Vec2): Int          = (other.x - x).abs + (other.y - y).abs
    infix def min(other: Vec2): Vec2   = append(other, _ min _)
    infix def max(other: Vec2): Vec2   = append(other, _ max _)

    private inline def append(other: Vec2, f: (Int, Int) => Int): Vec2 = (f(x, other.x), f(y, other.y))

    def dotProduct: Long            = x.toLong * y
    def neighbours: Vector[Vec2]    = CardinalDirections.map(vec + _)
    def allNeighbours: Vector[Vec2] = AllDirections.map(vec + _)

    def cw: Vec2     = (-y, x)
    def ccw: Vec2    = (y, -x)
    def negate: Vec2 = (-x, -y)

    def cw2: Vec2  = cwMap(vec)
    def ccw2: Vec2 = ccwMap(vec)

    infix def to(other: Vec2): Iterator[Vec2] =
      Iterator.iteropt(vec): vec =>
        (vec != other).option(vec + (other - vec).sign)
