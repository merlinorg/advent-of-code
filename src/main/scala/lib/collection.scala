package org.merlin.aoc
package lib

import impl.TupleOps.*
import number.*
import scala.collection.parallel.ParIterable

object collection:

  trait Collection[F[_]]:
    def iterator[A](fa: F[A]): Iterator[A]
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B
    def takeWhile[A](fa: F[A])(p: A => Boolean): F[A]

  given [F <: Iterable] => Collection[F]:
    override def iterator[A](fa: F[A]): Iterator[A] = fa.iterator

    override def map[A, B](fa: F[A])(f: A => B): F[B] = fa.map(f).asInstanceOf[F[B]]

    override def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B = fa.foldLeft(z)(f)

    override def takeWhile[A](fa: F[A])(p: A => Boolean): F[A] = fa.takeWhile(p).asInstanceOf[F[A]]
//
//  given Collection[Iterator]:
//    override def map[A, B](fa: Iterator[A])(f: A => B): Iterator[B] = fa.map(f)
//
//    override def foldLeft[A, B](fa: Iterator[A], z: B)(f: (B, A) => B): B = fa.foldLeft(z)(f)
//
//    override def takeWhile[A](fa: Iterator[A])(p: A => Boolean): Iterator[A] = fa.takeWhile(p)
//
//  given Collection[Array]:
//    override def map[A, B](fa: Array[A])(f: A => B): Array[B] = fa.map(f)
//
//    override def foldLeft[A, B](fa: Array[A], z: B)(f: (B, A) => B): B = fa.foldLeft(z)(f)
//
//    override def takeWhile[A](fa: Array[A])(p: A => Boolean): Array[A] = fa.takeWhile(p)

  // how to properly support String, Map

  extension [A](self: ParIterable[A])
    def sumMap[B: Numeric](f: A => B): B = self.map(f).sum
    def productMap[B: Numeric](f: A => B): B = self.map(f).product

  // TODO: split into foldable vs functor
  extension [A, F[_]](using F: Collection[F])(self: F[A])
    private def mapReduce[B](map: A => B, z: B, reduce: (B, B) => B): B =
      F.foldLeft(self, z)((b, a) => reduce(b, map(a)))

    private def mapReduceOpt[B](map: A => B, reduce: (B, B) => B): Option[B] =
      F.foldLeft(self, Option.empty[B])((acc, a) => acc.map(b => reduce(b, map(a))).orElse(Some(map(a))))

    def minMap[B: {Numeric as N, Extrema as X}](f: A => B): B = self.mapReduce(f, X.max, N.min)

    def maxMap[B: {Numeric as N, Extrema as X}](f: A => B): B = self.mapReduce(f, X.min, N.max)

    def sumMap[B: Numeric as N](f: A => B): B = self.mapReduce(f, N.zero, N.plus)

    def productMap[B: Numeric as N](f: A => B): B = self.mapReduce(f, N.one, N.times)

    def rangeMap[B: {Numeric as N, Extrema as X}](f: A => B): (B, B) =
      F.foldLeft(self, (X.max, X.min)): (range, a) =>
        val b = f(a)
        range.bimap(N.min(_, b), N.max(_, b))

    def foldCollect[B](z: B)(pf: PartialFunction[(B, A), B]): B =
      F.foldLeft(self, z)((b, a) => pf.lift(b -> a).getOrElse(b))

    def sumCollect[B: Numeric as N](pf: PartialFunction[A, B]): B =
      F.foldLeft(self, N.zero)((acc, a) => pf.lift(a).fold(acc)(N.plus(acc, _)))

    def minCollect[B: {Numeric as N, Extrema as X}](pf: PartialFunction[A, B]): B =
      F.foldLeft(self, X.max)((acc, a) => pf.lift(a).fold(acc)(N.min(acc, _)))

    def mapToMap[B, C](f: A => (B, C)): Map[B, C] =
      F.foldLeft(self, Map.empty[B, C])((acc, a) => acc + f(a))

    def sumToMap[B, C: Numeric as N](f: A => (B, C)): Map[B, C] =
      F.foldLeft(self, Map.empty[B, C]): (acc, a) =>
        val (b, c) = f(a)
        acc.updatedWith(b)(_.map(N.plus(_, c)).orElse(Some(c)))

    def mapTo[B](f: A => B): Map[A, B] =
      F.foldLeft(self, Map.empty[A, B])((acc, a) => acc + (a -> f(a)))

    def collectToMap[B, C](pf: PartialFunction[A, (B, C)]): Map[B, C] =
      F.foldLeft(self, Map.empty[B, C])((acc, a) => pf.lift(a).fold(acc)(acc + _))

    def collectToMultimap[B, C](pf: PartialFunction[A, (B, C)]): Map[B, Vector[C]] =
      F.foldLeft(self, Map.empty[B, Vector[C]]): (acc, a) =>
        pf.lift(a)
          .fold(acc): (b, c) =>
            acc.updatedWith(b):
              case None    => Some(Vector(c))
              case Some(v) => Some(v :+ c)

    def collectToSet[B](pf: PartialFunction[A, B]): Set[B] =
      F.foldLeft(self, Set.empty[B])((acc, a) => pf.lift(a).fold(acc)(acc + _))
    
    def toBag: Map[A, Int] = F.foldLeft(self, Map.empty[A, Int]): (acc, v) =>
      acc.updatedWith(v)(o => Some(o.fold(1)(_ + 1)))

    def toMultimap[B, C](using ABC: A <:< (B, C)): Map[B, Vector[C]] =
      F.foldLeft(self, Map.empty[B, Vector[C]]): (acc, a) =>
        val (b, c) = ABC(a)
        acc.updatedWith(b):
          case None    => Some(Vector(c))
          case Some(v) => Some(v :+ c)

    def countA(a: A): Int = F.foldLeft(self, 0)((acc, a0) => if a == a0 then acc + 1 else acc)

    def lcm(using L: A <:< Long): Long =
      F.foldLeft(self, 1L)((a, b) => b * a / a.gcd(L(b)))

    def fornone(p: A => Boolean): Boolean = F.iterator(self).forall(a => !p(a))

    def findFirst(p: A => Boolean): A = F.iterator(self).find(p).get

    def findMap[B](f: A => Option[B]): B = F.iterator(self).flatMap(f).next()

    def findMapOpt[B](f: A => Option[B]): Option[B] = F.iterator(self).flatMap(f).nextOption()

    def takeTo(p: A => Boolean): F[A] =
      var okay = true
      F.takeWhile(self): a =>
        val prior = okay
        okay = p(a)
        prior

    def takeUntil(p: A => Boolean): F[A] = F.takeWhile(self)(a => !p(a))

    def fproduct[B](f: A => B): F[(A, B)] = F.map(self)(a => (a, f(a)))

    def strengthL[B](b: B): F[(B, A)] = F.map(self)(b -> _)

    def strengthR[B](b: B): F[(A, B)] = F.map(self)(_ -> b)

//    def head2: (A, A) = (self.head, self.tail.head)

//    def pairs: Iterable[(A, A)] = self.grouped(2).map(a => a.head -> a.tail.head).toVector
//
//  def slidingPairs: Iterable[(A, A)] = if self.isEmpty then Nil else self.zip(self.tail)
//
//  def allPairs: Vector[(A, A)] = self.tails.toVector.tail.flatMap(self.zip)
//
//  def middle: A = self.drop(self.size / 2).head
//
//  def cross[B](other: Iterable[B]): Iterable[(A, B)] =
//    self.flatMap(a => other.map(a -> _))
//
