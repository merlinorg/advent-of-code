package org.merlin.aoc
package lib.impl

import LongOps.*

object IterableOps:
  extension [A](self: Iterable[A])
    def countA(a: A): Int = self.count(_ == a)

    def fornone(f: A => Boolean): Boolean = self.forall(a => !f(a))

    def findFirst(f: A => Boolean): A = self.find(f).get

    def findMap[B](f: A => Option[B]): B = self.iterator.flatMap(f).next()

    def findMapOpt[B](f: A => Option[B]): Option[B] = self.iterator.flatMap(f).nextOption()

    def fproduct[B](f: A => B): Iterable[(A, B)] = self.map(a => (a, f(a)))

    def strengthL[B](b: B): Iterable[(B, A)] = self.map(b -> _)

    def strengthR[B](b: B): Iterable[(A, B)] = self.map(_ -> b)

    def pair: (A, A) = (self.head, self.tail.head)

    def pairs: Iterable[(A, A)] = self.grouped(2).map(a => a.head -> a.tail.head).toVector

    def slidingPairs: Iterable[(A, A)] = if self.isEmpty then Nil else self.zip(self.tail)

    def allPairs: Vector[(A, A)] = self.tails.toVector.tail.flatMap(self.zip)

    def middle: A = self.drop(self.size / 2).head

    def cross[B](other: Iterable[B]): Iterable[(A, B)] =
      self.flatMap(a => other.map(a -> _))

    def lcm(using L: A <:< Long): Long =
      self.foldLeft(1L)((a, b) => b * a / a.gcd(L(b)))
