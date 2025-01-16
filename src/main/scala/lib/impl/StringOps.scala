package org.merlin.aoc
package lib.impl

import lib.impl.TupleOps.*

object StringOps:
  extension (self: String)
    def tuple2: (String, String) =
      self.span(_ != ' ').rmap(_.stripLeading)

    def linesv: Vector[String]   =
      self.linesIterator.toVector

    def integers: Vector[Int] =
      Parser.NumRE.findAllIn(self).map(_.toInt).toVector
