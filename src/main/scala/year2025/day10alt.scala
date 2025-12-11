package org.merlin.aoc
package year2025
package day10alt

import lib.{*, given}

import optimus.algebra.{Const, Constraint, Expression}
import optimus.optimization.*
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPIntVar

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

// https://github.com/vagmcs/Optimus
def part2(input: String): Long =
  val machines = input.parse
  machines.sumMap:
    case (_, buttons, joltage) =>
      given MPModel = MPModel(SolverLib.oJSolver)
      try
        val buttonVars = buttons.mapTo(_ => MPIntVar(0 to 1000))

        minimize(buttonVars.values.sumExpr)
        subjectTo:
          // Joltage_j = Sum(ButtonPresses_i Where Button_i increments Joltage_j)
          joltage.zipWithIndex.map: (v, j) =>
            buttons.filter(_(j)).map(buttonVars).sumExpr := Const(v)
        start()

        objectiveValue.round
      finally release()

def subjectTo(constraints: Iterable[Constraint])(using M: MPModel): Unit = constraints.foreach(M.add)

extension (self: Iterable[Expression])
  def sumExpr: Expression =
    self.tail.foldLeft[Expression](self.head)(_ + _)

extension (self: String)
  def parse: Vector[(lights: Set[Int], buttons: Vector[Set[Int]], joltages: Vector[Int])] = self.linesv.collect:
    case s"[$str] $buttons {${IV(jolts)}}" =>
      (str.findIndices('#').toSet, buttons.split(" ").toVector.map(_.integers.toSet), jolts)
