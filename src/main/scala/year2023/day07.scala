package org.merlin.aoc
package year2023
package day07

import lib.impl.IO.{*, given}
import lib.legacy.{*, given}
import scala.math.Ordering.Implicits.seqOrdering

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

private def f(lines: Vector[String], strengthF: String => List[Int]): Long =
  val hands  = lines.map:
    case s"$hand $bid" => hand -> bid.toLong
  val sorted = hands.map(_.lmap(strengthF)).sortBy(_._1)
  sorted.map(_._2).zipWithIndex.foldMap((bid, rank) => (rank + 1) * bid)

private val strengthsA = "23456789TJQKA".toList

private def strengthA(hand: String): List[Int] =
  strengthsA.map(c => hand.count(_ == c)).sorted.reverse ::: hand.toList.map(
    strengthsA.indexOf
  )

def part1(lines: Vector[String]): Long = f(lines, strengthA)

private val strengthsB = "J23456789TQKA".toList

private def strengthB(hand: String): List[Int] =
  val jokers :: cards = strengthsB.map(c => hand.count(_ == c)): @unchecked
  val best :: rest    = cards.sorted.reverse: @unchecked
  jokers + best :: rest ::: hand.toList.map(strengthsB.indexOf)

def part2(lines: Vector[String]): Long = f(lines, strengthB)
