package org.merlin.aoc
package year2019
package day23

import lib.{*, given}

@main def part1(): Unit =
  println(part1(actual))

@main def part2(): Unit =
  println(part2(actual))

val actual: String = load("actual.txt")

def part1(input: String): Long =
  network(input)
    .findMap: (_, nat, _, _, _) =>
      nat.lastOption

def part2(input: String): Long =
  network(input)
    .findMap: (_, _, _, lastY, priorY) =>
      Option.when(lastY == priorY)(lastY)

def network(input: String): Iterator[(Vector[Computer], Vector[Long], Int, Long, Long)] =
  val computers = (0 until 50).toVector.map(i => Computer(input, Vector(i)))
  Iterator
    .iterate((computers, Vector.empty[Long], 0, -1L, -2L)): (computers, nat, idle, lastY, priorY) =>
      val computers1             = computers.map(_.step.get)
      val computerOutputs        = computers1.map: computer =>
        if computer.output.size < 3 then (Vector.empty, computer)
        else (computer.output, computer.copy(output = Vector.empty))
      val (outputs, computers2)  = computerOutputs.unzip.lmap(_.filter(_.nonEmpty))
      val inputs0                = outputs.foldLeft(Map.empty[Long, Vector[Long]]): (map, output) =>
        map.updatedWith(output.head):
          case None    => Some(output.tail)
          case Some(v) => Some(v ++ output.tail)
      val (inputs1, nextY, oldY) = if idle < 1000 then (inputs0, lastY, priorY) else (Map(0L -> nat), nat(1), lastY)
      val computers3             = computers2.zipWithIndex.map: (computer, index) =>
        computer.copy(input = computer.input ++ inputs1.getOrElse(index, Vector.empty))
      (computers3, inputs1.get(255).fold(nat)(_.takeRight(2)), if inputs1.nonEmpty then 0 else idle + 1, nextY, oldY)
