package org.merlin.aoc
package year2025
package day10

import lib.queue.*
import lib.{*, given}

import scala.collection.parallel.CollectionConverters.*

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long =
  input.parse.sumMap:
    case (lights, buttons, _) =>
      Queue.unfold(lights -> 0L):
        case (lights, presses) =>
          Either.when(lights.isEmpty)(presses):
            buttons.map: indices =>
              val updated = indices.foldLeft(lights): (lights, index) =>
                if lights(index) then lights - index else lights + index
              updated -> (presses + 1)

// Key insight: If there is a joltage J0 higher than another joltage J1, and there is
// only one button that can reduce J0 but not J1, then at some point you will have to
// push that button, so do it now. If there's no such button, abandon this track.
// Otherwise BFS all the pushes.
def part2(input: String): Long =
  val machines = input.parse
  machines.zipWithIndex.par.sumMap:
    case ((_, buttons, joltage), i) =>
      println(s"... $i / ${machines.size}")
      Queue.unfold((joltage, buttons, 0L)): (joltage, buttons, pushed) =>
        Either.when(joltage.sum == 0)(pushed):
          joltage.zipWithIndex.crossProduct
            .collect:
              case ((v0, j0), (v1, j1)) if v0 > v1 => buttons.filter(indices => indices(j0) && !indices(j1))
            .minByOption(_.size) match
            case Some(Vector())        => Seq.empty
            case Some(Vector(indices)) =>
              for joltage <- joltage.decrement(indices).toSeq
              yield (joltage, buttons, pushed + 1)
            case _                     =>
              for
                buttons <- buttons.tails.toSeq
                indices <- buttons.headOption
                joltage <- joltage.decrement(indices)
              yield (joltage, buttons, pushed + 1)

extension (self: Vector[Int])
  def decrement(indices: Set[Int]): Option[Vector[Int]] =
    Option(Vector.tabulate(self.size)(j => self(j) - (if indices(j) then 1 else 0))).filter(_.forall(_ >= 0))

extension (self: String)
  def parse: Vector[(lights: Set[Int], buttons: Vector[Set[Int]], joltages: Vector[Int])] = self.linesv.collect:
    case s"[$str] $buttons {${IV(jolts)}}" =>
      (str.findIndices('#').toSet, buttons.split(" ").toVector.map(_.integers.toSet), jolts)
