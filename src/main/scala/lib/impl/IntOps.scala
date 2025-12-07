package org.merlin.aoc
package lib.impl

import scala.annotation.{tailrec, targetName}
import scala.util.Try

object IntOps:
  extension (self: Int)
    @targetName("posMod")
    inline def %%(mod: Int): Int = ((self % mod) + mod) % mod

    @targetName("geLt")
    inline def >=<(n: Int): Boolean = self >= 0 && self < n

    @targetName("geLt")
    inline def >=<[A: Numeric as N](t: (A, A)): Boolean = self >= N.toLong(t._1) && self < N.toLong(t._2)

    @targetName("or")
    inline def ||(n: => Int): Int = if self != 0 then self else n

    infix def divMod(mod: Int): (Int, Int) = (self / mod, self % mod)

    inline def even: Boolean = self % 2 == 0

    infix def mid(n: Int): Int = (self + n) / 2

    @tailrec infix def gcd(y: Int): Int = if y == 0 then self else y.gcd(self % y)

    def big: BigInt = BigInt(self)

  private[impl] inline def posMod(a: Int, b: Int): Int = a %% b

  extension (self: BigInt.type) def unapply(s: String): Option[BigInt] = Try(BigInt(s)).toOption
