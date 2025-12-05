package org.merlin.aoc
package lib.impl

import scala.collection.immutable.NumericRange
import lib.{Dir, Vec2}

object Parser:
  val NumRE = "-?\\d+".r
  val WordRE = "\\w+".r

  object C:
    def unapply(string: String): Option[Char] = Option.when(string.length == 1)(string.head)

  object D:
    def unapply(string: String): Option[Vec2] = Dir.get(string(0))

  object I:
    def unapply(string: String): Option[Int] = string.toIntOption

  object IV:
    def unapply(string: String): Option[Vector[Int]] = Some(NumRE.findAllIn(string).map(_.toInt).toVector)

  object L:
    def unapply(string: String): Option[Long] = string.toLongOption

  object LV:
    def unapply(string: String): Option[Vector[Long]] = Some(NumRE.findAllIn(string).map(_.toLong).toVector)

  object LR:
    def unapply(string: String): Option[NumericRange[Long]] = PartialFunction.condOpt(string):
      case s"${L(a)}-${L(b)}" => a to b
