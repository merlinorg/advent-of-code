package org.merlin.aoc
package lib.impl

import scala.annotation.targetName

object LongOps:
  extension (self: Long)
    @targetName("posMod")
    inline def %%(mod: Int): Int = (((self % mod) + mod) % mod).toInt

    @targetName("geLt")
    inline def >=<(n: Long): Boolean = self >= 0 && self < n

    @targetName("or")
    inline def ||(n: => Long): Long = if self != 0 then self else n

    infix def divMod(mod: Long): (Long, Long) = (self / mod, self % mod)
