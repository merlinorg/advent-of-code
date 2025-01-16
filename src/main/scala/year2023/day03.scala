package org.merlin.aoc
package year2023
package day03

import lib.impl.IO.*
import scalaz.*
import Scalaz.*

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: Vector[String] = loadv("sample.txt")

val actual: Vector[String] = loadv("actual.txt")

private val RunRe = "\\d+|[^.]".r

private final case class Run(x: Int, y: Int, str: String):
  def isNum: Boolean = str.forall(_.isDigit)

  def toInt: Int = str.toInt

  def isAdjacent(r: Run): Boolean =
    (r.y - y).abs <= 1 && (x + str.length >= r.x && x <= r.x + r.str.length)
end Run

private def parse(lines: Vector[String]): Vector[Run] =
  for
    (line, y) <- lines.zipWithIndex
    mtch      <- RunRe.findAllMatchIn(line)
  yield Run(mtch.start, y, mtch.matched)

def part1(lines: Vector[String]): Long =
  val runs = parse(lines)

  val parts = runs.filter: run =>
    run.isNum && runs.exists(r => !r.isNum && r.isAdjacent(run))

  parts.map(_.toInt).sum

def part2(lines: Vector[String]): Long =
  val runs = parse(lines)

  val gears = for
    g  <- runs if g.str == "*"
    adj = runs.filter(r => r.isNum && r.isAdjacent(g))
    if adj.length == 2
  yield adj.map(_.toInt).product

  gears.sum
