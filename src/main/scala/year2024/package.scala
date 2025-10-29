package org.merlin.aoc
package year2024

import lib.legacy.*

import scala.annotation.targetName

// a board is a vector of strings

type Board = Vector[String]

extension (self: Board)
  def width                            = self.head.length
  def height                           = self.length
  def nw: Loc                          = Origin
  def se: Loc                          = Loc(width - 1, height - 1)
  def apply(loc: Loc): Char            = self(loc.y.toInt)(loc.x.toInt)
  def get(loc: Loc): Option[Char]      = Option.when(loc >=< self)(self(loc))
  def is(loc: Loc, c: Int): Boolean    = loc >=< self && self(loc) == c
  def is(a: Loc, b: Loc): Boolean      = get(a) == get(b)
  def loc(char: Char): Loc             = locations.find(apply(_) == char).get
  def findAll(char: Char): Vector[Loc] = locations.filter(apply(_) == char)

  def locations: Vector[Loc] =
    self.indices.toVector.flatMap(y => self.head.indices.map(x => Loc(x, y)))

  def update(loc: Loc, c: Char): Vector[String] =
    self.updated(loc.y.toInt, self(loc.y.toInt).updated(loc.x.toInt, c))

  def split: (Board, Board) =
    val index = self.indexWhere(_.isEmpty)
    self.slice(0, index) -> self.slice(1 + index, self.length)

enum Dir(val dx: Long, val dy: Long):
  def cw: Dir             = Dir.fromOrdinal((ordinal + 2) % Dir.values.length)
  def ccw: Dir            = Dir.fromOrdinal((ordinal + Dir.values.length - 2) % Dir.values.length)
  def ccw2: Dir           = Dir.fromOrdinal((ordinal + Dir.values.length - 1) % Dir.values.length)
  def reverse: Dir        = Dir.fromOrdinal((ordinal + 4) % Dir.values.length)
  def horizontal: Boolean = dy == 0
  def vertical: Boolean   = dx == 0

  inline def *(length: Long): Vec = Vec(this, length)

  case N  extends Dir(0, -1)
  case NE extends Dir(1, -1)
  case E  extends Dir(1, 0)
  case SE extends Dir(1, 1)
  case S  extends Dir(0, 1)
  case SW extends Dir(-1, 1)
  case W  extends Dir(-1, 0)
  case NW extends Dir(-1, -1)

object Dir:
  val byName: Map[String, Dir] = Map("R" -> Dir.E, "D" -> Dir.S, "L" -> Dir.W, "U" -> Dir.N).withDefault(valueOf)

  given Ordering[Dir] = Ordering.by(_.ordinal)

val CardinalDirections: Vector[Dir] = Vector(Dir.N, Dir.E, Dir.S, Dir.W)

val OrdinalDirections: Vector[Dir] = Vector(Dir.NE, Dir.SE, Dir.SW, Dir.NW)

// a location in space

final case class Loc(x: Long, y: Long):
  inline def +(addend: Vec): Loc = Loc(x + addend.dx, y + addend.dy)
  inline def +(addend: Dir): Loc = Loc(x + addend.dx, y + addend.dy)

  inline def -(subtrahend: Vec): Loc = Loc(x - subtrahend.dx, y - subtrahend.dy)
  inline def -(subtrahend: Dir): Loc = Loc(x - subtrahend.dx, y - subtrahend.dy)

  inline def -(subtrahend: Loc): Loc = Loc(x - subtrahend.x, y - subtrahend.y)

  inline def +(addend: Loc): Loc = Loc(x + addend.x, y + addend.y)

  inline def >=<(board: Board): Boolean = this >=< (board.head.length, board.length)

  inline def <>=(board: Board): Boolean = !(this >=< board)

  inline def >=<(size: Long): Boolean = this >=< (size, size)

  inline def >=<(w: Long, h: Long): Boolean = x >=< w && y >=< h

  def adjacents: Vector[Loc] = CardinalDirections.map(this + _)

  def manhattan(l: Loc): Long = (l.x - x).abs + (l.y - y).abs

  @targetName("manhattanMoves") def +->(dst: Loc): Vector[Dir] =
    Option.when(x != dst.x)(if dst.x < x then Dir.W else Dir.E).toVector ++
      Option.when(y != dst.y)(if dst.y < y then Dir.N else Dir.S)

  override def toString: String = s"$x,$y"

given Ordering[Loc] = Ordering.by(Tuple.fromProductTyped)

val Origin: Loc = Loc(0, 0)

// a direction and magnitude

final case class Vec(direction: Dir, magnitude: Long):
  def dx: Long = direction.dx * magnitude
  def dy: Long = direction.dy * magnitude

  inline def +(addend: Long): Vec     = copy(magnitude = magnitude + addend)
  inline def -(subtrahend: Long): Vec = copy(magnitude = magnitude - subtrahend)
  inline def *(multiplier: Long): Vec = copy(magnitude = magnitude * multiplier)

// geometries

extension (self: Vector[Vec])
  def vertices: Vector[Loc] = self.scanLeft(Origin)(_ + _)
  def perimeter: Long       = self.map(_.magnitude).sum

extension (self: Vector[Loc]) def area: Long = self.zip(self.tail).map((a, b) => a.x * b.y - b.x * a.y).sum.abs / 2
