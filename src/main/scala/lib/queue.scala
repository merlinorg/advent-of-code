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
    def unfoldOpt[A, B](a: A*)(f: A => Either[Seq[A], B]): Option[B] =
      val queue  = mutable.Queue(a*)
      var result = Option.empty[B]
      while queue.nonEmpty && result.isEmpty do
        f(queue.dequeue()) match
          case Right(b) => result = Some(b)
          case Left(a)  => queue.enqueueAll(a)
      result

    def unfold[A, B](a: A*)(f: A => Either[Seq[A], B]): B = unfoldOpt(a*)(f).get

    def unfoldU[A](a: A)(f: A => Iterable[A]): Unit =
      val queue = mutable.Queue(a)
      queue.obliterator.foreach: a =>
        queue.enqueueAll(f(a))
 
  // see also:
  //  def bfsMap[A, B](a: A)(f: A => Either[Iterable[A], B]): Vector[B] =
  //    bfsImpl[A, B, mutable.ArrayBuffer[B]](a, mutable.ArrayBuffer.empty[B], _ += _)(f).toVector
    
  trait Priority[A]:
    type Dedup
    def ordering: Ordering[A]
    def dedup(a: A): Option[Dedup]

  object Priority:
    /** Prefer higher numbers. */
    def most[A, B: Ordering, C](f: A => B, g: A => C): Priority[A] = new Impl(Ordering.by(f), a => Some(g(a)))
    def most[A, B: Ordering](f: A => B): Priority[A]               = new Impl(Ordering.by(f), _ => None)

    /** Prefer lower numbers. */
    def least[A, B: Ordering, C](f: A => B, g: A => C): Priority[A] = new Impl(Ordering.by(f).reverse, a => Some(g(a)))
    def least[A, B: Ordering](f: A => B): Priority[A]               = new Impl(Ordering.by(f).reverse, _ => None)

    private class Impl[A, D](ord: Ordering[A], f: A => Option[D]) extends Priority[A]:
      override type Dedup = D
      override def ordering: Ordering[A]  = ord
      override def dedup(a: A): Option[D] = f(a)

  object PriorityQueue:
    def unfold[A, B](a: A*)(using P: Priority[A])(f: A => Either[Seq[A], B]): B =
      val queue  = mutable.PriorityQueue(a*)(using P.ordering)
      val seen   = mutable.Set.empty[Any]
      var result = Option.empty[B]
      while queue.nonEmpty && result.isEmpty do
        val a = queue.dequeue()
        if P.dedup(a).forall(seen.add) then
          f(a) match
            case Right(b) => result = Some(b)
            case Left(a)  => queue.enqueue(a*)
      result.get

    def unfoldU[A: Ordering](a: IterableOnce[A])(f: A => Seq[A]): Unit =
      val queue = mutable.PriorityQueue.from(a)
      queue.obliterator.foreach: a =>
        queue.enqueue(f(a)*)
