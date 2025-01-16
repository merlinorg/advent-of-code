package org.merlin.aoc
package lib.impl

object BooleanOps:
  extension (self: Boolean)
    def option[A](a: => A): Option[A] = Option.when(self)(a)
    def noption[A](a: => A): Option[A] = Option.when(!self)(a)
    def flatOption[A](a: => Option[A]): Option[A] = if self then a else None