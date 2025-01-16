package org.merlin.aoc
package lib.impl

// TODO: thtis should be on IteratableOps with a specialized return type
object SeqOps:
  extension [A](seq: Seq[A])
    def takeUntil(f: A => Boolean): Seq[A] =
      var okay = true
      seq.takeWhile: a =>
        val prior = okay
        okay = f(a)
        prior
