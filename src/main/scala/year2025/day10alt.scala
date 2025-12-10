package org.merlin.aoc
package year2025
package day10alt

import lib.queue.*
import lib.{*, given}

import org.apache.commons.math3.optim.MaxIter
import org.apache.commons.math3.optim.linear.{
  LinearConstraint,
  LinearConstraintSet,
  LinearObjectiveFunction,
  NonNegativeConstraint,
  Relationship,
  SimplexSolver
}
import org.apache.commons.math3.optim.nonlinear.scalar.GoalType

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long = day10.part1(input)

def part2(input: String): Long =
  val machines = input.parse
  machines.zipWithIndex.sumMap:
    case ((_, buttons, joltage), i) =>
      println(s"... $i / ${machines.size}")

      // Apache works but isn't integer program, always returns fractions

      val f           = new LinearObjectiveFunction(Array.fill(buttons.length)(1d), 0d)
      val constraints = Array.tabulate(joltage.size): i =>
        val Av = Array.tabulate(buttons.size): j =>
          if buttons(j)(i) then 1.0 else 0.0
        new LinearConstraint(Av, Relationship.EQ, joltage(i))

      val solver   = new SimplexSolver()
      val optimal  = solver.optimize(
        new MaxIter(100),
        f,
        new LinearConstraintSet(constraints*),
        GoalType.MINIMIZE,
        new NonNegativeConstraint(true),
      )
      val solution = optimal.getPoint
      println(solution.mkString(", "))

      // answer:
      // 1, 3, 0, 3, 1, 2

      solution.sum.toLong

type Matrix = Vector[Vector[Int]]

extension (self: Matrix)
  def stringify: String = self.map(_.map(_.toString.padTo(5, ' ')).mkString).mkString("", "\n", "\n")

extension (self: String)
  def parse: Vector[(lights: Set[Int], buttons: Vector[Set[Int]], joltages: Vector[Int])] = self.linesv.collect:
    case s"[$str] $buttons {${IV(jolts)}}" =>
      (str.findIndices('#').toSet, buttons.split(" ").toVector.map(_.integers.toSet), jolts)
