package org.merlin.aoc
package lib

object map:
  extension [A, B: Numeric as N](self: Map[A, B])
    def plus(a: A, b: B): Map[A, B] = self.updatedWith(a):
      case None     => Some(b)
      case Some(b2) => Some(N.plus(b2, b))
