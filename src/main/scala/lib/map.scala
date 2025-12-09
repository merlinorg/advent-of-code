package org.merlin.aoc
package lib

object map:
  extension [A, B](self: Map[A, B])
    def plus(a: A, b: B)(using N: Numeric[B]): Map[A, B] = self.updatedWith(a):
      case None     => Some(b)
      case Some(b2) => Some(N.plus(b2, b))

    def union[C](a: A, c: C)(using S: B =:= Set[C]): Map[A, B] = self.updatedWith(a):
      case None    => Some(S.flip(Set(c)))
      case Some(b) => Some(S.flip(S(b) + c))

    def append[C](a: A, c: C)(using V: B =:= Vector[C]): Map[A, B] = self.updatedWith(a):
      case None    => Some(V.flip(Vector(c)))
      case Some(b) => Some(V.flip(V(b) :+ c))
