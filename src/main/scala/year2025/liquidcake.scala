package org.merlin.aoc
package year2025
package liquidcake

import lib.{*, given}

// https: //pub.colonq.computer/~liquidcake/2025-puz/

@main def part1(): Unit =
  println(Power(2).candidates(4))
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = loadf("year2025/liquidcake/sample.txt")
val actual: String = loadf("year2025/liquidcake/actual.txt")

def part1(input: String): Long =
  val (grid, rules, soln) = input.parse

  rules.sumMap: (char, rule) =>
    val value = soln.number(grid.charIndices(char))
    if rule(value) then 0 else value

def part2(input: String): Long =
  val (grid, rules, soln) = input.parse

  val correct = rules
    .flatMap: (char, rule) =>
      val value = soln.number(grid.charIndices(char))
      Option.when(rule(value)):
        char -> value
    .toMap

  val expandedRules = rules
    .map: (char, rule) =>
      val indices    = grid.charIndices(char)
      val candidates = correct.get(char).fold(rule.candidates(indices.length))(Vector(_))
      candidates.map(n => indices.zip(n.toString))
    .sortBy(_.size)

  def solve(rules: Vector[Iterable[Vector[(Vec2, Char)]]], map: Map[Vec2, Char]): Option[Map[Vec2, Char]] =
    rules match
      case candidates +: rest =>
        candidates
          .filter: candidate =>
            candidate.forall: (loc, chr) =>
              map.get(loc).forall(_ == chr)
          .findMapOpt: candidate =>
            solve(rest, map ++ candidate)
      case _                  => Some(map)

  val result = solve(expandedRules, Map.empty).get.toGrid

  result
    .flatMap: row =>
      row.toVector.selectSplit(_.asDigit % 2 == 1)
    .sumMap: odd =>
      odd.mkString.toLong

extension (self: Vector[String])
  def charIndices(char: Char): Vector[Vec2] =
    Iterator
      .iteropt(self.gridIndex(char.toLower)): loc =>
        val next = (loc + (if char.isLower then East else South)) % self
        Option.when(self.is(next, '.'))(next)
      .toVector

  def number(indices: Vector[Vec2]): Long =
    indices.foldLeft(0L): (acc, pos) =>
      acc * 10 + self(pos).asDigit

extension (self: String)
  def parse: (Vector[String], Vector[(Char, Rule)], Vector[String]) =
    val chunks = self.linesv.chunks
    val rules  = chunks(1).collect:
      case s"${C(c)} is a square"              => (c, Exponent(2))
      case s"${C(c)} is a cube"                => (c, Exponent(3))
      case s"${C(c)} is a palindrome"          => (c, Palindrome)
      case s"${C(c)} is a multiple of ${L(n)}" => (c, Multiple(n))
      case s"${C(c)} is a power of ${I(n)}"    => (c, Power(n))
    (chunks(0), rules, chunks(2))

sealed trait Rule extends (Long => Boolean):
  def candidates(len: Int): Iterable[Long]

case class Exponent(exp: Int) extends Rule:
  override def apply(l: Long): Boolean              = Math.pow(l.toDouble, 1.0 / exp).toLong ** exp == l
  override def candidates(len: Int): Iterable[Long] =
    val min   = 10L ** (len - 1)
    val max   = (10L ** len) - 1
    val start = Math.ceil(Math.pow(min.toDouble, 1.0 / exp)).toLong
    val stop  = Math.floor(Math.pow(max.toDouble, 1.0 / exp)).toLong
    (start to stop).map(i => i ** exp)

case object Palindrome extends Rule:
  override def apply(l: Long): Boolean              = l.toString == l.toString.reverse
  override def candidates(len: Int): Iterable[Long] =
    val mid = (len + 1) / 2
    val min = 10L ** (mid - 1)
    val max = (10L ** mid) - 1
    (min to max).map: n =>
      val s = n.toString
      (s ++ s.reverse.drop(len % 2)).toLong

case class Multiple(n: Long) extends Rule:
  override def apply(l: Long): Boolean              = l % n == 0
  override def candidates(len: Int): Iterable[Long] =
    val min   = 10L ** (len - 1)
    val max   = (10L ** len) - 1
    val start = Math.ceil(min.toDouble / n).toLong
    val stop  = Math.floor(max.toDouble / n).toLong
    (start to stop).map(i => i * n)

case class Power(n: Int) extends Rule:
  override def apply(l: Long): Boolean              = n ** (Math.log(l.toDouble) / Math.log(n)).toInt == l
  override def candidates(len: Int): Iterable[Long] =
    val min   = 10L ** (len - 1)
    val max   = (10L ** len) - 1
    val start = Math.ceil(Math.log(min.toDouble) / Math.log(n.toDouble)).toInt
    val stop  = Math.floor(Math.log(max.toDouble) / Math.log(n.toDouble)).toInt
    (start to stop).map(i => n ** i)
