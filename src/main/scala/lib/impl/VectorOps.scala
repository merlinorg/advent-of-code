package org.merlin.aoc
package lib.impl

import lib.impl.TupleOps.*

object VectorOps:
  extension [A](self: Vector[A])
    def updatedWith(i: Int)(f: A => A): Vector[A] =
      self.updated(i, f(self(i)))

    def middle: A = self(self.length / 2)

    def get(i: Int): Option[A] = Option.when(i >= 0 && i < self.length)(self(i))

    def splice(from: Int, length: Int, insert: Vector[A] = Vector.empty): Vector[A] =
      self.slice(0, from) ++ insert ++ self.slice(from + length, self.length)

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

    def split1: (A, Vector[A]) =
      self.head -> self.tail

    // maps a vector with an accumulator, returning the final accumulator and values
    def mapAcc[B, C](c0: C)(f: (C, A) => (C, B)): (C, Vector[B]) =
      self.foldLeft(c0 -> Vector.empty[B]):
        case ((c, bs), a) => f(c, a) match { case (c2, b) => (c2, bs :+ b) }

    // stateful map, maps a vector with an accumulator then drops the accumulator at the end
    def mapS[B, C](c0: C)(f: (C, A) => (C, B)): Vector[B] = mapAcc(c0)(f)._2

    def *(n: Int): Vector[A] = (0 until n).flatMap(_ => self).toVector

    def getOrElse(i: Int, a: => A): A =
      if i >= 0 && i < self.size then self(i) else a

  extension (self: Vector[String])
    def chunks: Vector[Vector[String]] =
      Iterator
        .unfold(self): v =>
          Option.when(v.nonEmpty):
            v.bichunk
        .toVector

    def bichunk: (Vector[String], Vector[String]) =
      self.span(_.nonEmpty).rmap(_.dropWhile(_.isEmpty))

    def toLongs: Vector[Long] = self.map(_.toLong)
