package org.merlin.aoc
package lib.impl

import scala.collection.mutable
import lib.collection.*
import lib.memo.*
import lib.queue.*

object BFS:
  def shortestPath[A](start: A, endF: A => Boolean, neighbourF: A => Iterable[A]): Option[Vector[A]] =
    val visited = mutable.Set(start)
    val queue   = mutable.Queue(Vector(start))

    while queue.nonEmpty && !endF(queue.head.last) do
      val path       = queue.dequeue()
      val neighbours = neighbourF(path.last).filter(visited.add)
      queue.addAll(neighbours.map(path :+ _))

    queue.headOption

  def countPaths[A](start: A, endF: A => Boolean, neighbourF: A => Iterable[A]): Long =
    memoized[Long](start): (loc, loop) =>
      if endF(loc) then 1 else neighbourF(loc).sumMap(loop)

  def minimumDistances[A](routes: Map[A, Iterable[A]]): Map[A, Map[A, Int]] =
    routes.keys.mapToMap: a =>
      val distances = mutable.Map.empty[A, Int]
      val queue     = mutable.Queue(a -> 0)
      queue.obliterator.foreach: (loc, distance) =>
        if !distances.contains(loc) then
          if distance > 0 then distances.update(loc, distance)
          queue.enqueueAll:
            routes.get(loc).iterator.flatMap(_.strengthR(distance + 1))
      a -> distances.toMap

  def floodfill[A](start: A, neighbourF: A => Vector[A]): Set[A] =
    val visited = mutable.Set(start)
    Queue.unfoldU(start): current =>
      neighbourF(current).filter(visited.add)
    visited.toSet
