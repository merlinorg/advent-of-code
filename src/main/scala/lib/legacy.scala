package org.merlin.aoc
package lib
package legacy

import scala.annotation.targetName
import scala.collection.immutable.NumericRange
import scala.collection.mutable

export lib.collection.{*, given}

export lib.number.{*, given}

export lib.impl.IntOps.*

export lib.impl.LongOps.*

export lib.impl.Parser.*

export lib.impl.StringOps.*

export lib.impl.BooleanOps.*

export lib.impl.IterableOps.*

export lib.impl.IteratorOps.*

export lib.impl.TupleOps.*

export lib.impl.OptionOps.*

export lib.impl.VectorOps.*

// id extensions
extension [A](self: A)
  @targetName("bird")
  def |>[B](f: A => B): B = f(self)

// range extensions

extension (self: NumericRange[Long])
  def splitLess(limit: Long): (NumericRange[Long], NumericRange[Long]) =
    self.splitAt((limit - self.head).toInt)

  def splitGreater(limit: Long): (NumericRange[Long], NumericRange[Long]) =
    self.splitAt((1 + limit - self.head).toInt).swap

  def range: Long = if self.isEmpty then 0 else 1 + self.last - self.head

private def bfsImpl[A, B, C](a: A, z: C, append: (C, B) => C)(f: A => Either[Iterable[A], B]): C =
  var result = z
  val queue  = mutable.Queue(a)
  while queue.nonEmpty do
    f(queue.dequeue()) match
      case Right(r)     => result = append(result, r)
      case Left(states) => queue.enqueueAll(states)
  result

import lib.fp.Monoid

def bfsFoldl[A, B : Monoid as M](a: A)(f: A => Either[Iterable[A], B]): B =
  bfsImpl(a, M.zero, M.combine)(f)
