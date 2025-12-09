package org.merlin.aoc
package lib

object number:
  class Extrema[A](val min: A, val max: A)

  given Extrema[Long] = new Extrema(Long.MinValue, Long.MaxValue)
  given Extrema[Int]  = new Extrema(Int.MinValue, Int.MaxValue)
  
