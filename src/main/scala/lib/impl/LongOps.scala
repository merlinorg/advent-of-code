package org.merlin.aoc
package lib.impl

import scala.annotation.{tailrec, targetName}

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
    inline def **(n: Int): Long = n match
      case 0 => 1
      case 1 => self
      case 2 => self * self
      case n => math.pow(self.doubleValue, n.doubleValue).longValue

    inline def digits: Int = 1 + math.log10(self.doubleValue).intValue

    @tailrec infix def gcd(y: Long): Long = if y == 0 then self else y.gcd(self % y)

    /** multiplicative inverse mod N, toth euler .. see also BigInt */
    def modInv(mod: Long): Long =
      @tailrec def loop(a: Long, b: Long, x0: Long, x1: Long): Long =
        if a > 1 then loop(b, a % b, x1 - a / b * x0, x0)
        else if x1 < 0 then x1 + mod
        else x1
      loop(self, mod, 0, 1)

    def big: BigInt = BigInt(self)
