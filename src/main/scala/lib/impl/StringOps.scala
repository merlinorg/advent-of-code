package org.merlin.aoc
package lib.impl

import lib.impl.IntOps.*
import lib.impl.TupleOps.*

object StringOps:
  extension (self: String)
    def tuple2: (String, String) =
      self.span(_ != ' ').rmap(_.stripLeading)

    def linesv: Vector[String] =
      self.stripSuffix("\n").linesIterator.toVector

    def integers: Vector[Int] =
      Parser.NumRE.findAllIn(self).map(_.toInt).toVector

    def longs: Vector[Long] =
      Parser.NumRE.findAllIn(self).map(_.toLong).toVector

    def words: Vector[String] =
      Parser.WordRE.findAllIn(self).toVector

    def findIndices(c: Char): Vector[Int] =
      self.indices.filter(self(_) == c).toVector

    def commaSeparated: Vector[String] = self.split(',').toVector

    def parseBinary: Int = Integer.parseInt(self, 2)

    def startsWith(c: Char): Boolean = self.nonEmpty && self.head == c
    
    def get(index: Int): Option[Char] =
      Option.when(index >=< self.length)(self(index))

    def dropPrefix(p: String): Option[String] = Option.when(self.startsWith(p))(self.drop(p.length))

    def rotateLeft(n: Int): String = self.drop(n) + self.take(n)
