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
      solve1(lights, buttons)

// if the joltages are all even then it takes twice the presses of half the joltages
def part2(input: String): Long =
  val machines = input.parse
  machines.par.sumMap:
    case (_, buttons, joltage) =>
      val permutations = buttons.foldLeft(Vector((0, Vector.fill(joltage.size)(0)))): (acc, indices) =>
        acc ++ acc.map((presses, decrements) => (presses + 1, decrements.plus(indices)))
      val result       = memoized[Option[Long]](joltage): (joltage, loop) =>
        if joltage.forall(_ == 0) then Some(0)
        else
          val results = for
            (presses, decrements) <- permutations // this should just be all the solve1s...
            remaining              = joltage.zip(decrements).map(_ - _)
            if remaining.forall(_ >= 0) && remaining.forall(_ % 2 == 0)
            cost                  <- loop(remaining.map(_ / 2))
          yield presses + 2 * cost
          results.minOption
      result.get

def solve1(lights: BitSet, buttons: Vector[BitSet]): Long =
  def loop(index: Int, lights: BitSet, presses: Long): Long =
    if lights.isEmpty then presses
    else if index == buttons.length then Long.MaxValue
    else loop(index + 1, lights, presses) min loop(index + 1, lights.xor(buttons(index)), presses + 1)
  loop(0, lights, 0L)

extension (self: Vector[Int])
  def plus(b: BitSet): Vector[Int] = Vector.tabulate(self.size)(i => self(i) + (if b(i) then 1 else 0))

extension (self: String)
  def parse: Vector[(lights: BitSet, buttons: Vector[BitSet], joltages: Vector[Int])] = self.linesv.collect:
    case s"[$str] $buttons {${IV(jolts)}}" =>
      (BitSet(str.findIndices('#')*), buttons.split(" ").toVector.map(s => BitSet(s.integers*)), jolts)
