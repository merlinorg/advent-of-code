package org.merlin.aoc
package lib.impl

import scala.collection.mutable

object MutableMapOps:
  extension [A, B](self: mutable.Map[A, B]) def memo(a: A)(b: => B): B = self.getOrElseUpdate(a, b)
