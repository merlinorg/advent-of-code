package org.merlin.aoc
package lib.impl

import lib.impl.TupleOps.*

object StringVectorOps:
  import Vec2Ops.*

  extension (self: Vector[String])
    def chunks: Vector[Vector[String]] =
      Iterator
        .unfold(self): v =>
          Option.when(v.nonEmpty):
            v.bichunk
        .toVector

    def bichunk: (Vector[String], Vector[String]) =
      self.span(_.nonEmpty).rmap(_.dropWhile(_.isEmpty))

    def toLongs: Vector[Long] = self.map(_.toLong)
