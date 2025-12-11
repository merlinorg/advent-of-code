package org.merlin.aoc
package year2025
package day10alt

import lib.{*, given}

import org.chocosolver.solver.Model

import scala.collection.parallel.CollectionConverters.*

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long = day10.part1(input)

// https://choco-solver.org/
def part2(input: String): Long =
  val machines = input.parse
  machines.zipWithIndex.par.sumMap:
    case ((_, buttons, joltage), i) =>
      println(s"... $i / ${machines.size}")
      val model      = new Model()
      val buttonVars = model.intVarArray("b", buttons.size, 0, 1000)
      joltage.zipWithIndex.foreach: (v, j) =>
        // Joltage_j = Sum(ButtonPresses_i Where Button_i increments Joltage_j)
        val buttonIndices = buttons.indices.filter(buttons(_)(j))
        model.sum(buttonIndices.map(buttonVars).toArray, "=", v).post()
      val pressesVar = model.intVar("presses", 0, 10000)
      // Presses = Sum(ButtonPresses_i)
      model.sum(buttonVars, "=", pressesVar).post()
      // Solve for minimum presses
      val maximize   = false
      val solution   = model.getSolver.findOptimalSolution(pressesVar, maximize)
      solution.getIntVal(pressesVar)

extension (self: String)
  def parse: Vector[(lights: Set[Int], buttons: Vector[Set[Int]], joltages: Vector[Int])] = self.linesv.collect:
    case s"[$str] $buttons {${IV(jolts)}}" =>
      (str.findIndices('#').toSet, buttons.split(" ").toVector.map(_.integers.toSet), jolts)
