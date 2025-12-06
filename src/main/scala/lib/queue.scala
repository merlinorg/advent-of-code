package org.merlin.aoc
package lib

import scala.collection.mutable

object queue:

  extension [A](self: mutable.Queue[A])
    def obliterator: Iterator[A] = new Iterator[A]:
      override def hasNext: Boolean = self.nonEmpty
      override def next(): A        = self.dequeue()

  extension [A](self: mutable.PriorityQueue[A])
    def obliterator: Iterator[A] = new Iterator[A]:
      override def hasNext: Boolean = self.nonEmpty
      override def next(): A        = self.dequeue()

  object Queue:
    def unfoldU[A](a: A)(f: A => Iterable[A]): Unit =
      val queue = mutable.Queue(a)
      queue.obliterator.foreach: a =>
        queue.enqueueAll(f(a))

  object PriorityQueue:
    def unfold[A: Ordering, B](a: A*)(f: A => Either[Seq[A], B]): B =
      val queue  = mutable.PriorityQueue(a*)
      var result = Option.empty[B]
      while queue.nonEmpty && result.isEmpty do
        f(queue.dequeue()) match
          case Right(b) => result = Some(b)
          case Left(a)  => queue.enqueue(a*)
      result.get

    def unfoldU[A: Ordering](a: IterableOnce[A])(f: A => Seq[A]): Unit =
      val queue = mutable.PriorityQueue.from(a)
      queue.obliterator.foreach: a =>
        queue.enqueue(f(a)*)
