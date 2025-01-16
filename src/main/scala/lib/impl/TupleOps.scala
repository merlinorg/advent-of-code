package org.merlin.aoc
package lib.impl

object TupleOps:
  extension [A, B](self: (A, B))
    def lmap[C](fa: A => C): (C, B)                 = (fa(self(0)), self(1))
    def rmap[C](fb: B => C): (A, C)                 = (self(0), fb(self(1)))
    def bimap[C, D](fa: A => C, fb: B => D): (C, D) = (fa(self(0)), fb(self(1)))
