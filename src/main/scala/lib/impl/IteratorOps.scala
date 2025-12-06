package org.merlin.aoc
package lib.impl

import scala.collection.AbstractIterator
import scala.compiletime.uninitialized

object IteratorOps:
  extension [A](self: Iterator[A])
    def minMap(f: A => Long): Long =
      self.foldLeft(Long.MaxValue)((acc, a) => acc min f(a))

    def maxMap(f: A => Long): Long =
      self.foldLeft(Long.MinValue)((acc, a) => acc max f(a))

    def sumMap[N: Numeric as N](f: A => N): N =
      self.foldLeft(N.zero)((sum, a) => N.plus(sum, f(a)))

    def findFirst(f: A => Boolean): A = self.find(f).get

    def findMap[B](f: A => Option[B]): B =
      self.flatMap(f).next()

    def findMapOpt[B](f: A => Option[B]): Option[B] =
      self.flatMap(f).nextOption()

    def findCollect[B](f: PartialFunction[A, B]): B = findMap(f.lift)

    def foldCollect[B](z: B)(pf: PartialFunction[(B, A), B]): B =
      self.foldLeft(z)((b, a) => pf.lift(b -> a).getOrElse(b))

    def sumCollect(pf: PartialFunction[A, Long]): Long =
      self.foldLeft(0L)((sum, a) => pf.lift(a).fold(sum)(sum + _))

    def collectToSet[B](pf: PartialFunction[A, B]): Set[B] =
      self.collect(pf).toSet

    def slidingPairs: Iterator[(A, A)] = self.sliding(2).map(a => a.head -> a.tail.head)

    def last(): A =
      var result = self.next()
      while self.hasNext do result = self.next()
      result

    def last[B](f: A => B): B = f(last())

    def nth(n: Int): A = self.drop(n).next

    def nth[B](n: Int, f: A => B): B = f(nth(n))

    def countUntil(p: A => Boolean): Long =
      var result = 0L
      while self.hasNext && !p(self.next()) do result = result + 1
      result

    /** Takes until but excluding when a predicate matches. */
    def takeUntil(p: A => Boolean): Iterator[A] = self.takeWhile(a => !p(a))

    /** Takes until and including when a predicate matches. */
    def takeTo(p: A => Boolean): Iterator[A] = new AbstractIterator[A]:
      private var hd: A              = uninitialized
      private var hdDefined: Boolean = false
      private var tail: Iterator[A]  = self

      def hasNext: Boolean = hdDefined || tail.hasNext && {
        hd = tail.next()
        hdDefined = true
        if p(hd) then tail = Iterator.empty
        true
      }

      def next(): A = if hasNext then
        hdDefined = false
        hd
      else Iterator.empty.next()

  extension (self: Iterator.type)
    def iteropt[A](init: A)(f: A => Option[A]): Iterator[A] =
      Iterator.single(init) ++ Iterator.unfold(init): a =>
        f(a).map(a => (a, a))

    def itercol[A](init: A)(f: PartialFunction[A, A]): Iterator[A] =
      Iterator.single(init) ++ Iterator.unfold(init): a =>
        f.unapply(a).map(a => (a, a))

    def last[A](init: A)(f: A => Option[A]): A =
      iteropt(init)(f).last()

    def repeat[A](values: Iterable[A]): Iterator[A] =
      Iterator.continually(values).flatten