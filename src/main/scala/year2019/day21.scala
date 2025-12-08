package org.merlin.aoc
package year2019
package day21

import lib.{*, given}
import lib.queue.*

@main def part1(): Unit =
  println(part1(actual))

@main def part2(): Unit =
  println(part2(actual))

val actual: String = load("actual.txt")

def part1(input: String): Long =
  // Asimov's laws of robotics: Jump if you must, don't jump if you can't
  val initial = Vector(
    // <- search here for when else to jump
    "AND D J", // nothing in D then don't jump
    "NOT A T", // nothing in A then jump
    "OR T J",
    "WALK"
  )

  // These searches are generally intractable 30^N, so we need to provide as much help as we can
  Queue.unfold(initial): program =>
    val output = Computer(input, program.mkString("", "\n", "\n").asciiLongs).unfoldIO.last()
    Either.when(output >= 128)(output):
      for
        op <- Seq("NOT", "OR", "AND")
        x  <- Seq("B", "C", "T") // exclude sensors we've used
        y  <- Seq("J", "T")
      yield s"$op $x $y" +: program

def part2(input: String): Long =
  val initial = Vector(
    "NOT B J", // jump if hole at B
    "NOT C T", // or..
    "OR T J",  // if hole at C
    // <- search here for when to not jump
    "AND D J", // nothing in D then don't jump
    "NOT A T", // nothing in A then jump
    "OR T J",
    "RUN"
  )
  // println:(Computer(input, initial.mkString("", "\n", "\n").asciiLongs).runIOs.map(_.toChar).mkString)
  Queue.unfold(initial): program =>
    val output = Computer(input, program.mkString("", "\n", "\n").asciiLongs).unfoldIO.last()
    Either.when(output >= 128)(output):
      for
        x  <- Seq("E", "F", "G", "H", "I", "T") // exclude sensors we've used
        op <- Seq("NOT", "AND", "OR")
        y  <- Seq("J", "T")
      yield program.splice(3, 0, s"$op $x $y")
