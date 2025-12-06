package org.merlin.aoc
package lib

import scala.annotation.tailrec

object interval:
  type Interval = (Long, Long)

  extension (self: Set[Interval])
    @tailrec infix def union(interval: Interval): Set[Interval] =
      self.find(_ intersects interval) match
        case None        => self + interval
        case Some(other) => (self - other) union (interval union other)

    def contains(a: Long): Boolean = self.exists(_.contains(a))

  extension (self: Interval)
    infix def intersects(other: Interval): Boolean =
      self._1 <= other._2 && self._2 >= other._1

    infix def union(other: Interval): Interval =
      (self._1 min other._1) -> (self._2 max other._2)

    def contains(a: Long): Boolean = a >= self._1 && a <= self._2

    def range: Long = self._2 - self._1 + 1

  object Interval:
    def unapply(string: String): Option[(Long, Long)] = PartialFunction.condOpt(string):
      case s"${L(a)}-${L(b)}" => a -> b
