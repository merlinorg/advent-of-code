package org.merlin.aoc
package lib

import scala.collection.mutable

object memo:
  
  extension [A, B](self: mutable.Map[A, B]) def memo(a: A)(b: => B): B = self.getOrElseUpdate(a, b)

  def memoize[A, B](f: A => B): A => B = f.memoized

  /** Usage: <pre> val b: B = memoized[B](a0: A): (a: A, f: A => B) => B </pre>
    */
  def memoized[B] = new Memoizer[B]

  class Memoizer[B]:
    def apply[A](a: A)(f: (A, A => B) => B): B = Memoize(f)(a)

  /** Usage: <pre> val b: B = memoizedFind[B](a0: A): (a: A, f: A => Option[B]) => B </pre>
    */
  def memoizedFind[B] = new FindMemoizer[B]

  class FindMemoizer[B]:
    def apply[A](a: A)(f: (A, A => Option[B]) => Option[B]): B = Memoize(f)(a).get

  private class Memoize[A, B](f: (A, A => B) => B) extends (A => B):
    val cache = mutable.Map.empty[A, B]

    def apply(a: A): B = cache.memo(a)(f(a, this))

  object Memoize:
    def apply[A, B](f: (A, A => B) => B): A => B = new Memoize[A, B](f)

  extension [A, B](self: A => B)
    def memoized: A => B =
      val memo = mutable.Map.empty[A, B]
      a => memo.getOrElseUpdate(a, self(a))