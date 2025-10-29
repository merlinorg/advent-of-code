package org.merlin.aoc
package year2023

import lib.legacy.*

// a board is a vector of strings

type Board = Vector[String]

extension (self: Board)
  def width                       = self.head.length
  def height                      = self.length
  def nw: Loc                     = Origin
  def se: Loc                     = Loc(width - 1, height - 1)
  def apply(loc: Loc): Char       = self(loc.y.toInt)(loc.x.toInt)
  def get(loc: Loc): Option[Char] = Option.when(loc >=< self)(self(loc))

  def find(char: Char): Loc = locations.find(apply(_) == char).get

  def locations: Vector[Loc] =
    self.indices.toVector.flatMap(y => self.head.indices.map(x => Loc(x, y)))

  def split: (Board, Board) =
    val index = self.indexWhere(_.isEmpty)
    self.slice(0, index) -> self.slice(1 + index, self.length)

// the cardinal directions

enum Dir(val dx: Long, val dy: Long):
  def cw: Dir      = Dir.fromOrdinal((ordinal + 1) % 4)
  def ccw: Dir     = Dir.fromOrdinal((ordinal + Dir.values.length - 1) % 4)
  def reverse: Dir = Dir.fromOrdinal((ordinal + 2) % 4)

  inline def *(length: Long): Vec = Vec(this, length)

  case N extends Dir(0, -1)
  case E extends Dir(1, 0)
  case S extends Dir(0, 1)
  case W extends Dir(-1, 0)

object Dir:
  val byName: Map[String, Dir] = Map("R" -> Dir.E, "D" -> Dir.S, "L" -> Dir.W, "U" -> Dir.N).withDefault(valueOf)

  given Ordering[Dir] = Ordering.by(_.ordinal)

// a location in space

final case class Loc(x: Long, y: Long):
  inline def +(addend: Vec): Loc = Loc(x + addend.dx, y + addend.dy)
  inline def +(addend: Dir): Loc = Loc(x + addend.dx, y + addend.dy)

  inline def -(subtrahend: Vec): Loc = Loc(x - subtrahend.dx, y - subtrahend.dy)

  inline def >=<(board: Board): Boolean = x >=< board.head.length && y >=< board.length

  def adjacents: Vector[Loc] = Dir.values.map(this + _).toVector

  def manhattan(l: Loc): Long = (l.x - x).abs + (l.y - y).abs

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
