package org.merlin.aoc
package lib.impl

import scala.annotation.targetName

object IntOps:
  extension (self: Int)
    @targetName("posMod")
    inline def %%(mod: Int): Int = ((self % mod) + mod) % mod

    @targetName("geLt")
    inline def >=<(n: Int): Boolean = self >= 0 && self < n

    @targetName("or")
    inline def ||(n: => Int): Int = if self != 0 then self else n

    infix def divMod(mod: Int): (Int, Int) = (self / mod, self % mod)

    inline def even: Boolean = self % 2 == 0

    infix def mid(n: Int): Int = (self + n) / 2

  private[impl] inline def posMod(a: Int, b: Int): Int = a %% b