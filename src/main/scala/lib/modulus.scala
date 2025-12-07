package org.merlin.aoc
package lib

import scala.annotation.tailrec

object modulus:
  opaque type BigMod = BigInt

  extension (self: BigMod)
    /** This as a positive integer value. */
    def toIndex(using M: BigMod): Int          = ((self + M) % M).toInt
    def toLong: Long                           = self.toLong
    def +(o: Long)(using M: BigMod): BigMod    = (self + o).mod(M)
    def -(o: Long)(using M: BigMod): BigMod    = (self - o).mod(M)
    def *(o: Long)(using M: BigMod): BigMod    = (self * o).mod(M)
    def **(o: Long)(using M: BigMod): BigMod   = self.modPow(o, M)
    def +(o: BigMod)(using M: BigMod): BigMod  = (self + o).mod(M)
    def -(o: BigMod)(using M: BigMod): BigMod  = (self - o).mod(M)
    def *(o: BigMod)(using M: BigMod): BigMod  = (self * o).mod(M)
    def **(o: BigMod)(using M: BigMod): BigMod = self.modPow(o, M)
    def modInv(using M: BigMod): BigMod        = self.modInverse(M)
    def unary_- : BigMod                       = -self

    /** `self + self.b + self.b^2 + ... + self.b^(n - 1)`. Runs in O(log(n)). */
    def geometricSum(b: BigMod, n: Long)(using M: BigMod): BigMod =
      @tailrec def loop(t: BigInt, e: BigInt, n: Long, total: BigInt): BigInt =
        if n == 0 then (self * total).mod(M)
        else
          val nextTotal = if n % 2 == 1 then (total * e + t).mod(M) else total
          loop((t * (e + 1)).mod(M), (e * e).mod(M), n / 2, nextTotal)
      loop(BigMod.one, b, n, BigMod.zero)

  object BigMod:
    val zero: BigMod                       = BigInt(0)
    val one: BigMod                        = BigInt(1)
    def apply(l: Long): BigMod             = BigInt(l)
    def unapply(s: String): Option[BigMod] = BigInt.unapply(s)
