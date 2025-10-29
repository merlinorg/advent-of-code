package org.merlin.aoc
package lib.impl

object OptionOps:
  extension [A](self: Option[A])
    def strengthL[B](b: => B): Option[(B, A)] = self.map(b -> _)
    def strengthR[B](b: => B): Option[(A, B)] = self.map(_ -> b)
    def as[B](b: => B): Option[B]             = self.map(_ => b)
    def |(a: => A): A                         = self.getOrElse(a)
    def ||(a: => Option[A]): Option[A]        = self.orElse(a)
    def cata[B](f: A => B, z: => B): B        = self.fold(z)(f)
