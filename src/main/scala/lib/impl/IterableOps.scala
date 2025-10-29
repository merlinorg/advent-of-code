package org.merlin.aoc
package lib.impl

object IterableOps:
  extension [A](self: Iterable[A])
    def minMap[B](f: A => B)(using N: Numeric[B]): B =
      self.foldLeft(Option.empty[B])((acc, a) => acc.map(b => N.min(b, f(a))).orElse(Some(f(a)))).get

    def maxMap[B](f: A => B)(using N: Numeric[B]): B =
      self.foldLeft(Option.empty[B])((acc, a) => acc.map(b => N.max(b, f(a))).orElse(Some(f(a)))).get

    def sumMap(f: A => Long): Long =
      self.foldLeft(0L)((acc, a) => acc + f(a))

    def productMap(f: A => Long): Long =
      self.foldLeft(1L)((acc, a) => acc * f(a))

    def sumCollect(pf: PartialFunction[A, Long]): Long =
      self.foldLeft(0L)((acc, a) => pf.lift(a).fold(acc)(acc + _))

    def minCollect(pf: PartialFunction[A, Long]): Long =
      self.foldLeft(Long.MaxValue)((acc, a) => pf.lift(a).fold(acc)(acc min _))

    def mapToMap[B, C](f: A => (B, C)): Map[B, C] =
      self.foldLeft(Map.empty[B, C])((acc, a) => acc + f(a))

    def mapTo[B](f: A => B): Map[A, B] =
      self.foldLeft(Map.empty[A, B])((acc, a) => acc + (a -> f(a)))

    def collectToMap[B, C](pf: PartialFunction[A, (B, C)]): Map[B, C] =
      self.foldLeft(Map.empty[B, C])((acc, a) => pf.lift(a).fold(acc)(acc + _))

    def collectToSet[B](pf: PartialFunction[A, B]): Set[B] =
      self.collect(pf).toSet

    def findMap[B](f: A => Option[B]): B = self.iterator.flatMap(f).next()

    def findMapOpt[B](f: A => Option[B]): Option[B] = self.iterator.flatMap(f).nextOption()

    def fproduct[B](f: A => B): Iterable[(A, B)] = self.map(a => (a, f(a)))

    def strengthL[B](b: B): Iterable[(B, A)] = self.map(b -> _)

    def strengthR[B](b: B): Iterable[(A, B)] = self.map(_ -> b)

    def pairs: Iterable[(A, A)] = self.grouped(2).map(a => a.head -> a.tail.head).toVector

    def slidingPairs: Iterable[(A, A)] = self.sliding(2).map(a => a.head -> a.tail.head).toVector
