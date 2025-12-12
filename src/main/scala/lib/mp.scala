package org.merlin.aoc
package lib

import ch.qos.logback.classic.{Level, Logger}
import optimus.algebra.{Constraint, Expression}
import optimus.optimization.enums.SolverLib
import optimus.optimization.enums.SolverLib.oJSolver
import optimus.optimization.{MPModel, objectiveValue, start}
import org.slf4j.Logger.ROOT_LOGGER_NAME
import org.slf4j.LoggerFactory

object mp:
  // so loud
  LoggerFactory.getLogger(ROOT_LOGGER_NAME).asInstanceOf[Logger].setLevel(Level.WARN)

  def solveUsingModel(solverLib: SolverLib = oJSolver)(f: MPModel ?=> Unit): Long =
    usingModel(solverLib):
      val _ = f
      start()
      objectiveValue.round

  def usingModel[B](solverLib: SolverLib = oJSolver)(f: MPModel ?=> B): B =
    val model = MPModel(solverLib)
    try f(using model)
    finally model.release()

  extension (self: MPModel) def subjectTo(constraints: Iterable[Constraint]): Unit = constraints.foreach(self.add)

  extension (self: Iterable[Expression]) def sumExpr: Expression = self.reduce(_ + _)
