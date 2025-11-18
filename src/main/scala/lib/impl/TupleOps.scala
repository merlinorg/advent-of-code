package org.merlin.aoc
package lib.impl

object TupleOps:
  extension [A, B](self: (A, B))
    inline def fold[C](f: (A, B) => C): C                  = f(self._1, self._2)
    inline def lmap[C](fa: A => C): (C, B)                 = (fa(self(0)), self(1))
    inline def rmap[C](fb: B => C): (A, C)                 = (self(0), fb(self(1)))
    inline def bimap[C, D](fa: A => C, fb: B => D): (C, D) = (fa(self(0)), fb(self(1)))

  extension [A](self: (A, A))
    def sum(using N: Numeric[A]): A = N.plus(self._1, self._2)
    def difference(using N: Numeric[A]): A = N.minus(self._2, self._1)
    def product(using N: Numeric[A]): A = N.times(self._1, self._2)

  extension [A](self: (A, A, A))
    inline def set(i: 0 | 1 | 2, a: A): (A, A, A) =
      if i == 0 then (a, self(1), self(2))
      else if i == 1 then (self(0), a, self(2))
      else (self(0), self(1), a)
    inline def get(i: Int): A = self(i).asInstanceOf[A] // when called with a constant, inlines to ._N