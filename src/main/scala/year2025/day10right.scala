package org.merlin.aoc
package year2025
package day10right

import lib.memo.memoized
import lib.{*, given}

import scala.collection.immutable.BitSet
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
      solve1(lights, buttons).minMap(_.size)

// if the joltages are all even then it takes twice the presses of half the joltages
def part2(input: String): Long =
  input.parse.par.sumMap:
    case (_, buttons, joltage) =>
      val result = memoized[Option[Long]](joltage): (joltage, loop) =>
        if joltage.forall(_ == 0) then Some(0)
        else
          val results = for
            presses  <- solve1(joltage.oddBits, buttons)
            remaining = presses.toVector.map(buttons).foldLeft(joltage)(_ -- _) if remaining.forall(_ >= 0)
            halfCost <- loop(remaining.map(_ / 2))
          yield presses.size + 2 * halfCost
          results.minOption
      result.get

def solve1(lights: BitSet, buttons: Vector[BitSet]): Vector[BitSet] =
  def loop(index: Int, lights: BitSet, presses: BitSet): Vector[BitSet] =
    val self = if lights.isEmpty then Vector(presses) else Vector.empty
    if index == buttons.length then self
    else self ++ loop(index + 1, lights, presses) ++ loop(index + 1, lights.xor(buttons(index)), presses + index)
  loop(0, lights, BitSet.empty)

extension (self: Vector[Int])
  def oddBits: BitSet            = BitSet.fromSpecific(self.zipWithIndex.flatMap((v, i) => Option.when(v.odd)(i)))
  def --(b: BitSet): Vector[Int] = Vector.tabulate(self.size)(i => self(i) + (if b(i) then -1 else 0))

extension (self: String)
  def parse: Vector[(lights: BitSet, buttons: Vector[BitSet], joltages: Vector[Int])] = self.linesv.collect:
    case s"[$str] $buttons {${IV(jolts)}}" =>
      (BitSet(str.findIndices('#')*), buttons.split(" ").toVector.map(s => BitSet(s.integers*)), jolts)
