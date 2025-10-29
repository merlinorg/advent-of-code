package org.merlin.aoc
package lib
package legacy

import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.NumericRange
import scala.collection.mutable

// algebra

trait Semigroup[A]:
  def combine(a0: A, a1: A): A

trait Monoid[A] extends Semigroup[A]:
  def zero: A

object Monoid:
  def instance[A](z: A, f: (A, A) => A): Monoid[A] = new Monoid[A]:
    override def zero: A                  = z
    override def combine(a0: A, a1: A): A = f(a0, a1)

given NumericMonoid: [A: Numeric as N] => Monoid[A] = Monoid.instance(N.zero, N.plus)

given MapMonoid: [A, B: Semigroup as S] => Monoid[Map[A, B]]:
  def zero: Map[A, B] = Map.empty

  def combine(ab0: Map[A, B], ab1: Map[A, B]): Map[A, B] =
    ab1.foldLeft(ab0):
      case (ab, (a1, b1)) =>
        ab.updatedWith(a1):
          case Some(b0) => Some(S.combine(b0, b1))
          case None     => Some(b1)

given ListMonoid: [A] => Monoid[List[A]] = Monoid.instance(List.empty, _ ++ _)

given SetMonoid: [A] => Monoid[Set[A]] = Monoid.instance(Set.empty, _ ++ _)

given TupleMonoid: [A: Monoid as MA, B: Monoid as MB] => Monoid[(A, B)]:
  def zero: (A, B)                            = (MA.zero, MB.zero)
  def combine(a0: (A, B), a1: (A, B)): (A, B) = (MA.combine(a0._1, a1._1), MB.combine(a0._2, a1._2))

extension [A: Semigroup as S](self: A) def |+|(a: A): A = S.combine(self, a)

// id extensions
extension [A](self: A)
  @targetName("bird")
  def |>[B](f: A => B): B = f(self)

// boolean extensions

extension (self: Boolean) def ??[A](a: => A)(using M: Monoid[A]): A = if self then a else M.zero

export lib.impl.BooleanOps.*

// number extensions

export lib.impl.IntOps.*

export lib.impl.LongOps.*

val NumRe  = "-?\\d+".r
val WordRe = "\\S+".r

// string extensions

extension (self: String)
  def numbers: Vector[Long]                 = NumRe.findAllIn(self).map(_.toLong).toVector
  def words: Vector[String]                 = WordRe.findAllIn(self).toVector
  def commaSeparated: Vector[String]        = self.split(',').toVector
  def characters: Vector[String]            = self.split("").toVector
  def dropPrefix(p: String): Option[String] = Option.when(self.startsWith(p))(self.drop(p.length))

// iterable extensions

extension [A](self: Iterable[A])
  def foldLeftMap[B, C](z: B)(f: B => C)(op: (B, A) => B): C = f(self.foldLeft(z)(op))

  def suml(using M: Monoid[A]): A = self.foldLeft(M.zero)(M.combine)

  def foldMap[B](f: A => B)(using M: Monoid[B]) = self.foldLeft(M.zero): (b, a) =>
    M.combine(b, f(a))

export lib.impl.IterableOps.*

// iterator extensions

extension [A](self: Iterator[A])
  def foldMap[B: Numeric](f: A => B): B                    = self.map(f).sum
  def foldCollect[B: Numeric](f: PartialFunction[A, B]): B = self.flatMap(f.lift).sum

export lib.impl.IteratorOps.*

// map extensions

extension [A, B](self: Map[A, B]) def suml(using M: Monoid[B]): B = self.values.foldLeft(M.zero)(M.combine)

export lib.impl.MutableMapOps.*

// tuple extensions

export lib.impl.TupleOps.*

// option extensions

export lib.impl.OptionOps.*

// range extensions

extension (self: NumericRange[Long])
  def splitLess(limit: Long): (NumericRange[Long], NumericRange[Long]) =
    self.splitAt((limit - self.head).toInt)

  def splitGreater(limit: Long): (NumericRange[Long], NumericRange[Long]) =
    self.splitAt((1 + limit - self.head).toInt).swap

  def range: Long = if self.isEmpty then 0 else 1 + self.last - self.head

// vector extensions

extension [A](self: Vector[A])
  def flatFoldMap[B](f: A => Iterable[B])(using Monoid[B]): B = self.flatMap(f).suml

  def foldCollect[B](pf: PartialFunction[A, B])(using Monoid[B]): B = self.collect(pf).suml

  // maps a vector with an accumulator, returning the final accumulator and values
  def mapAcc[B, C](c0: C)(f: (C, A) => (C, B)): (C, Vector[B]) =
    self.foldLeft(c0 -> Vector.empty[B]):
      case ((c, bs), a) => f(c, a) match { case (c2, b) => (c2, bs :+ b) }

  // stateful map, maps a vector with an accumulator then drops the accumulator at the end
  def mapS[B, C](c0: C)(f: (C, A) => (C, B)): Vector[B] = mapAcc(c0)(f)._2

  def groupToMap[B, C](using ABC: A <:< (B, C)): Map[B, Vector[C]] = self
    .map(ABC)
    .foldLeft(Map.empty[B, Vector[C]]):
      case (bc, (b, c)) =>
        bc.updatedWith(b):
          case None     => Some(Vector(c))
          case Some(cs) => Some(cs :+ c)

  // all non-self element pairs
  def allPairs: Vector[(A, A)] = self.tails.toVector.tail.flatMap(self.zip)

  def slidingPairs: Vector[(A, A)] = self.sliding(2).map(s => (s(0), s(1))).toVector

  def splice(from: Int, length: Int, insert: Vector[A] = Vector.empty): Vector[A] =
    self.slice(0, from) ++ insert ++ self.slice(from + length, self.length)

  def middle: A = self(self.length / 2)

  def get(i: Int): Option[A] = Option.when(i >= 0 && i < self.length)(self(i))

  def groupWhen(pred: (A, A) => Boolean): Vector[Vector[A]] =
    self
      .foldLeft(Vector.empty[Vector[A]] -> Vector.empty[A]):
        case ((groups, next), a) =>
          if next.lastOption.forall(pred(_, a)) then (groups, next :+ a)
          else (groups :+ next, Vector(a))
      .fold:
        case (groups, Nil)  => groups
        case (groups, next) => groups :+ next

  def selectSplit(f: A => Boolean): Vector[Vector[A]] =
    self
      .foldLeft(Vector.empty[Vector[A]] -> Vector.empty[A]):
        case ((groups, next), a) =>
          if f(a) then (groups, next :+ a)
          else (if next.isEmpty then groups else groups :+ next, Vector.empty)
      .fold:
        case (groups, Nil)  => groups
        case (groups, next) => groups :+ next

extension (self: Vector[String]) def numbers: Vector[Vector[Long]] = self.map(_.numbers)

def Y[A, B](f: (A => B, A) => B, x: A): B =
  f(v => Y(f, v), x)

private def bfsImpl[A, B, C](a: A, z: C, append: (C, B) => C)(f: A => Either[Iterable[A], B]): C =
  var result = z
  val queue  = mutable.Queue(a)
  while queue.nonEmpty do
    f(queue.dequeue()) match
      case Right(r)     => result = append(result, r)
      case Left(states) => queue.enqueueAll(states)
  result

def bfsMap[A, B](a: A)(f: A => Either[Iterable[A], B]): Vector[B] =
  bfsImpl[A, B, mutable.ArrayBuffer[B]](a, mutable.ArrayBuffer.empty[B], _ += _)(f).toVector

def bfsFoldl[A, B](a: A)(f: A => Either[Iterable[A], B])(using M: Monoid[B]): B =
  bfsImpl(a, M.zero, M.combine)(f)

def lcm(list: Iterable[Long]): Long      = list.foldLeft(1L)((a, b) => b * a / gcd(a, b))
@tailrec def gcd(x: Long, y: Long): Long = if y == 0 then x else gcd(y, x % y)
