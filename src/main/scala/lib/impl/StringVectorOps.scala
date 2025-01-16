package org.merlin.aoc
package lib.impl

import lib.impl.TupleOps.*

object StringVectorOps:
  extension (vector: Vector[String])
    def chunks: Vector[Vector[String]] =
      Iterator
        .unfold(vector): v =>
          Option.when(v.nonEmpty):
            v.bichunk
        .toVector

    def bichunk: (Vector[String], Vector[String]) =
      vector.span(_.nonEmpty).rmap(_.dropWhile(_.isEmpty))

    def toLongs: Vector[Long] = vector.map(_.toLong)
