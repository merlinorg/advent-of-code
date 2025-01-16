package org.merlin.aoc
package lib.impl

object Vec3Ops:
  type Vec3 = (Int, Int, Int)

  val Up3D        = (0, -1, 0)
  val Down3D      = (0, 1, 0)
  val Left3D      = (-1, 0, 0)
  val Right3D     = (1, 0, 0)
  val Forwards3D  = (0, 0, -1)
  val Backwards3D = (0, 0, 1)

  val CardinalDirections3D = Vector(Up3D, Down3D, Left3D, Right3D, Forwards3D, Backwards3D)

  val Origin3D = (0, 0, 0)

  extension (vec: Vec3)
    def x: Int                       = vec(0)
    def y: Int                       = vec(1)
    def z: Int                       = vec(2)
    def +(other: Vec3): Vec3         = append(other, _ + _)
    def -(other: Vec3): Vec3         = append(other, _ - _)
    infix def min(other: Vec3): Vec3 = append(other, _ min _)
    infix def max(other: Vec3): Vec3 = append(other, _ max _)

    def neighbours: Vector[Vec3] = CardinalDirections3D.map(vec + _)

    private inline def append(other: Vec3, f: (Int, Int) => Int): Vec3 = (f(x, other.x), f(y, other.y), f(z, other.z))
