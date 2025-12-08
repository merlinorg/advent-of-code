package org.merlin.aoc
package year2024
package day22alt

import lib.{*, given}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample2))
  println(part2(actual))

val sample: String  = load("sample.txt")
val sample2: String = load("sample2.txt")
val actual: String  = load("actual.txt")

def part1(input: String): Long =
  input.linesIterator.foldMap: line =>
    line.toLong.secretsIterator.nth(2000)

def part2(input: String): Long =
  val deltaTotals = input.linesIterator.foldMap: line =>
    deltaMap(line)
  deltaTotals.values.max

def deltaMap(line: String): Map[(Long, Long, Long, Long), Long] =
  given Semigroup[Long] = leftBiasedSemigroup
  line.toLong.secretsIterator
    .map(_ % 10)
    .take(2000)
    .sliding(5)
    .foldMap: quintuple =>
      Map(deltaQuartuple(quintuple) -> quintuple(4))

def deltaQuartuple(q: Seq[Long]): (Long, Long, Long, Long) =
  (q(1) - q(0), q(2) - q(1), q(3) - q(2), q(4) - q(3))

extension (self: Long)
  inline def step(f: Long => Long): Long = mix(f(self)).prune
  inline def mix(n: Long): Long          = self ^ n
  inline def prune: Long                 = self % 16777216
  inline def nextSecret: Long            = step(_ * 64).step(_ / 32).step(_ * 2048)
  def secretsIterator: Iterator[Long]    = Iterator.iterate(self)(_.nextSecret)

trait Semigroup[A]:
  def combine(a0: A, a1: A): A

trait Monoid[A] extends Semigroup[A]:
  def zero: A

given NumericMonoid: [A: Numeric as N] => Monoid[A]:
  def zero: A                  = N.zero
  def combine(a0: A, a1: A): A = N.plus(a0, a1)

given MapMonoid: [A, B: Semigroup as S] => Monoid[Map[A, B]]:
  def zero: Map[A, B] = Map.empty

  def combine(ab0: Map[A, B], ab1: Map[A, B]): Map[A, B] =
    ab1.foldLeft(ab0):
      case (ab, (a1, b1)) =>
        ab.updatedWith(a1):
          case Some(b0) => Some(S.combine(b0, b1))
          case None     => Some(b1)

def leftBiasedSemigroup[A]: Semigroup[A] = (a0: A, _: A) => a0

extension [A](self: Iterator[A])
  def nth(n: Int): A =
    self.drop(n).next()

  def foldMap[B](f: A => B)(using M: Monoid[B]): B =
    self.map(f).foldLeft(M.zero)(M.combine)
