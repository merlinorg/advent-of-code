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

    def sumMap(f: A => Long): Long =
      self.foldLeft(0L)((sum, a) => sum + f(a))

    def findFirst(f: A => Boolean): A = self.find(f).get

    def findMap[B](f: A => Option[B]): B =
      self.flatMap(f).next()

    def findCollect[B](f: PartialFunction[A, B]): B = findMap(f.lift)

    def sumCollect(pf: PartialFunction[A, Long]): Long =
      self.foldLeft(0L)((sum, a) => pf.lift(a).fold(sum)(sum + _))

    def collectToSet[B](pf: PartialFunction[A, B]): Set[B] =
      self.collect(pf).toSet
      
    def last(): A =
      var result = self.next()
      while self.hasNext do result = self.next()
      result

    def nth(n: Int): A = self.drop(n).next

    def takeUntil(p: A => Boolean): Iterator[A] = new AbstractIterator[A]:
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

    def slidingPairs: Iterator[(A, A)] = self.sliding(2).map(a => a.head -> a.tail.head)

  extension (self: Iterator.type)
    def iteropt[A](init: A)(f: A => Option[A]): Iterator[A] =
      Iterator.single(init) ++ Iterator.unfold(init): a =>
        f(a).map(a => (a, a))
