package org.merlin.aoc
package lib.impl

object EitherOps:

  extension (self: Either.type)
    def right[L]: RightProjector[L] = new RightProjector

  class RightProjector[L]:
    def apply[R](r: R): Either[L, R] = Right(r)