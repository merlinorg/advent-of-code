package org.merlin.aoc
package year2025
package day10alt

import lib.{*, given}

import optimus.algebra.*
import optimus.optimization.*
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.{MPBinaryVar, MPIntVar}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

// https://github.com/vagmcs/Optimus

def part1(input: String): Long =
  val machines = input.parse
  machines.sumMap:
    case (lights, buttons, joltage) =>
      given MPModel = MPModel(SolverLib.oJSolver)
      try
        val buttonVars = buttons.mapTo(_ => MPBinaryVar())
        val parityVars = joltage.indices.map(_ => MPIntVar(0 to 100)) // toth @twentylemon

        minimize(buttonVars.values.sumExpr)
        subjectTo:
          // Sum(ButtonPresses_i Where Button_i increments Joltage_j) = 2 * Parity_j + Light_j for some unconstrained parity
          joltage.indices.map: j =>
            buttons.filter(_(j)).map(buttonVars).sumExpr := parityVars(j) * 2 + (if lights(j) then 1 else 0)
        start()

        objectiveValue.round
      finally release()

def part2(input: String): Long =
  val machines = input.parse
  machines.sumMap:
    case (_, buttons, joltage) =>
      given MPModel = MPModel(SolverLib.oJSolver)
      try
        val buttonVars = buttons.mapTo(_ => MPIntVar(0 to joltage.max))

        minimize(buttonVars.values.sumExpr)
        subjectTo:
          // Sum(ButtonPresses_i Where Button_i increments Joltage_j) = Joltage_j
          joltage.zipWithIndex.map: (v, j) =>
            buttons.filter(_(j)).map(buttonVars).sumExpr := Const(v)
        start()

        objectiveValue.round
      finally release()

def subjectTo(constraints: Iterable[Constraint])(using MP: MPModel): Unit = constraints.foreach(MP.add)

extension (self: Iterable[Expression]) def sumExpr: Expression = self.reduce(_ + _)

extension (self: String)
  def parse: Vector[(lights: Set[Int], buttons: Vector[Set[Int]], joltages: Vector[Int])] = self.linesv.collect:
    case s"[$str] $buttons {${IV(jolts)}}" =>
      (str.findIndices('#').toSet, buttons.split(" ").toVector.map(_.integers.toSet), jolts)
