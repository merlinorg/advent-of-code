package org.merlin.aoc
package lib.impl

object IterableOps:
  extension [A](self: Iterable[A])
    def minMap[B: Numeric as N](f: A => B): B =
      self.foldLeft(Option.empty[B])((acc, a) => acc.map(b => N.min(b, f(a))).orElse(Some(f(a)))).get

    def maxMap[B: Numeric as N](f: A => B): B =
      self.foldLeft(Option.empty[B])((acc, a) => acc.map(b => N.max(b, f(a))).orElse(Some(f(a)))).get

    def sumMap[B: Numeric as N](f: A => B): B =
      self.foldLeft(N.zero)((acc, a) => N.plus(acc, f(a)))

    def productMap[B: Numeric as N](f: A => B): B =
      self.foldLeft(N.one)((acc, a) => N.times(acc, f(a)))

    def sumCollect[B: Numeric as N](pf: PartialFunction[A, B]): B =
      self.foldLeft(N.zero)((acc, a) => pf.lift(a).fold(acc)(N.plus(acc, _)))

    def minCollect[B: Numeric as N](pf: PartialFunction[A, B]): B =
      self.foldLeft(Option.empty[B])((acc, a) => pf.lift(a).fold(acc)(b => acc.map(N.min(_, b)).orElse(Some(b)))).get

    def mapToMap[B, C](f: A => (B, C)): Map[B, C] =
      self.foldLeft(Map.empty[B, C])((acc, a) => acc + f(a))

    def sumToMap[B, C: Numeric as N](f: A => (B, C)): Map[B, C] =
      self.foldLeft(Map.empty[B, C]): (acc, a) =>
        val (b, c) = f(a)
        acc.updatedWith(b)(_.map(N.plus(_, c)).orElse(Some(c)))

    def mapTo[B](f: A => B): Map[A, B] =
      self.foldLeft(Map.empty[A, B])((acc, a) => acc + (a -> f(a)))

    def collectToMap[B, C](pf: PartialFunction[A, (B, C)]): Map[B, C] =
      self.foldLeft(Map.empty[B, C])((acc, a) => pf.lift(a).fold(acc)(acc + _))

    def collectToSet[B](pf: PartialFunction[A, B]): Set[B] =
      self.collect(pf).toSet

    def findFirst(f: A => Boolean): A = self.find(f).get

    def findMap[B](f: A => Option[B]): B = self.iterator.flatMap(f).next()

    def findMapOpt[B](f: A => Option[B]): Option[B] = self.iterator.flatMap(f).nextOption()

    def fproduct[B](f: A => B): Iterable[(A, B)] = self.map(a => (a, f(a)))

    def strengthL[B](b: B): Iterable[(B, A)] = self.map(b -> _)

    def strengthR[B](b: B): Iterable[(A, B)] = self.map(_ -> b)

    def pairs: Iterable[(A, A)] = self.grouped(2).map(a => a.head -> a.tail.head).toVector

    def allPairs: Iterable[(A, A)] = if self.isEmpty then Nil else self.zip(self.tail)

    def slidingPairs: Iterable[(A, A)] = self.sliding(2).map(a => a.head -> a.tail.head).toVector

    def toBag: Map[A, Int] = self.foldLeft(Map.empty[A, Int]): (acc, v) =>
      acc.updatedWith(v)(o => Some(o.fold(1)(_ + 1)))

    def middle: A = self.drop(self.size / 2).head

    def cross[B](other: Iterable[B]): Iterable[(A, B)] =
      self.flatMap(a => other.map(a -> _))
