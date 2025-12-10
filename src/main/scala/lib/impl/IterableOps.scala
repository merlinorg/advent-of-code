package org.merlin.aoc
package lib.impl

object IterableOps:
  extension [A](self: Iterable[A])
    def head2: (A, A) = (self.head, self.tail.head)

    def pairs: Iterable[(A, A)] = self.grouped(2).map(a => a.head -> a.tail.head).toVector

    def slidingPairs: Iterable[(A, A)] = if self.isEmpty then Nil else self.zip(self.tail)

    def allPairs: Vector[(A, A)] = self.tails.toVector.tail.flatMap(self.zip)

    def middle: A = self.drop(self.size / 2).head

    def cross[B](other: Iterable[B]): Iterable[(A, B)] =
      self.flatMap(a => other.map(a -> _))
