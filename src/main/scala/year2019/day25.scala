package org.merlin.aoc
package year2019
package day25

import lib.{*, given}

import scala.collection.mutable
import scala.io.StdIn

@main def part1(): Unit =
  println(part1(actual))

val actual: String = load("actual.txt")

val backups = mutable.Stack.empty[Computer]

def part1(input: String): Unit =
  Iterator
    .iterate(Computer(input)): computer =>
      val (output, computer1) = Iterator
        .iterate("" -> computer): (string, computer) =>
          computer.runIO match
            case Some((chr, computer)) =>
              (string.appended(chr.toChar), computer)
            case None                  =>
              backups.pop()
              ("Rebooting!\n", backups.head)
        .findFirst(_._1.endsWith("\n"))
      println(output)
      if output == "Command?\n" then
        val input     = StdIn.readLine("> ")
        val computer2 = computer1.copy(input = s"$input\n".asciiLongs)
        backups.push(computer2)
        computer2
      else computer1
    .last()
