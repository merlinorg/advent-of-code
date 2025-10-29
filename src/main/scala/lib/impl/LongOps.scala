package org.merlin.aoc
package lib.impl

import scala.annotation.targetName

object LongOps:
  extension (self: Long)
    @targetName("posModInt")
    inline def %%(mod: Int): Int = (self %% mod.toLong).toInt

    @targetName("posMod")
    inline def %%(mod: Long): Long = ((self % mod) + mod) % mod

    @targetName("geLt")
    inline def >=<(n: Long): Boolean = self >= 0 && self < n

    @targetName("geLt")
    inline def >=<[A: Numeric as N](t: (A, A)): Boolean = self >= N.toLong(t._1) && self < N.toLong(t._2)

    @targetName("or")
    inline def ||(n: => Long): Long = if self != 0 then self else n

    @targetName("divMod")
    infix def /%(mod: Long): (Long, Long) = (self / mod, self % mod)

    @targetName("absDiff")
    inline def |-|(n: Long): Long = (self - n).abs

    @targetName("pow")
    inline def **(n: Long): Long = math.pow(self.doubleValue, n.doubleValue).longValue

    inline def digits: Int = 1 + math.log10(self.doubleValue).intValue
