package org.merlin.aoc
package lib.impl

object EitherOps:

  extension (self: Either.type)
    def right[L]: RightProjector[L]                                       = new RightProjector
    def when[L, R](b: Boolean, ifTrue: => R)(ifFalse: => L): Either[L, R] =
      Either.cond(b, ifTrue, ifFalse)

  class RightProjector[L]:
    def apply[R](r: R): Either[L, R] = Right(r)
