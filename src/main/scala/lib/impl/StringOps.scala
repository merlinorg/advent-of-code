package org.merlin.aoc
package lib.impl

import lib.impl.TupleOps.*

object StringOps:
  extension (self: String)
    def tuple2: (String, String) =
      self.span(_ != ' ').rmap(_.stripLeading)

    def linesv: Vector[String] =
      self.linesIterator.toVector

    def integers: Vector[Int] =
      Parser.NumRE.findAllIn(self).map(_.toInt).toVector

    def words: Vector[String] =
      Parser.WordRE.findAllIn(self).toVector

    def parseBinary: Int = Integer.parseInt(self, 2)

    def startsWith(c: Char): Boolean = self.nonEmpty && self.head == c