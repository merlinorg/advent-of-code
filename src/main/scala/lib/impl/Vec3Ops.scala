package org.merlin.aoc
package lib.impl

import LongOps.*
import Parser.*

object Vec3Ops:
  type Vec3 = (Int, Int, Int)

  val Up3D        = (0, -1, 0)
  val Down3D      = (0, 1, 0)
  val Left3D      = (-1, 0, 0)
  val Right3D     = (1, 0, 0)
  val Forwards3D  = (0, 0, -1)
  val Backwards3D = (0, 0, 1)

  val CardinalDirections3D = Vector(Up3D, Down3D, Left3D, Right3D, Forwards3D, Backwards3D)

  val Origin3D: Vec3 = (0, 0, 0)

  extension (vec: Vec3)
    def x: Int         = vec(0)
    def y: Int         = vec(1)
    def z: Int         = vec(2)
    def magnitude: Int = x.abs + y.abs + z.abs
    def abs: Vec3      = (x.abs, y.abs, z.abs)
    def sign: Vec3     = (x.sign, y.sign, z.sign)

    def +(other: Vec3): Vec3         = append(other, _ + _)
    def -(other: Vec3): Vec3         = append(other, _ - _)
    def |-|(other: Vec3): Int        = (other.x - x).abs + (other.y - y).abs + (other.z - z).abs
    def <->(other: Vec3): Long       = (other.x - x) ** 2 + (other.y - y) ** 2 + (other.z - z) ** 2
    infix def min(other: Vec3): Vec3 = append(other, _ min _)
    infix def max(other: Vec3): Vec3 = append(other, _ max _)

    def neighbours: Vector[Vec3] = CardinalDirections3D.map(vec + _)

    private inline def append(other: Vec3, f: (Int, Int) => Int): Vec3 = (f(x, other.x), f(y, other.y), f(z, other.z))

    def roll: Vec3 = (x, z, -y)
    def turn: Vec3 = (-y, x, z)

    def rotations: Vector[Vec3] =
      (0 to 23).toVector.scanLeft(vec): (vec, i) =>
        if i == 12 then vec.roll.turn.roll.roll else if i % 4 == 0 then vec.roll else vec.turn

  object Vec3:
    def unapply(s: String): Option[Vec3] = PartialFunction.condOpt(s):
      case s"${I(x)},${I(y)},${I(z)}" => (x, y, z)
