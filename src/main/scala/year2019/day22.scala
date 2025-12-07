package org.merlin.aoc
package year2019
package day22

import lib.{*, given}

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main
def part2(): Unit =
//  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long =
  val n = if input.length < 1000 then 10 else 10007
  (0 until n).toVector
    .shuffle(input.parse)
    .indexOf(if input.length < 1000 then 6 else 2019)

def part2(input: String): Long = {
  val steps = input.parse.take(2)
  val n     = 10007L
  val deck  = (0 until n.toInt).toVector
  val x     = 10L
  (0 until 10).foldLeft(deck): (deck, i) =>
    println(s"$i: ${deck(x.toInt)}")
    deck.shuffle(steps)

  // these are coprime.. gcd 1 .. probably both prime
  // println(s"XXX ${119315717514047L.gcd(101741582076661L)}")

  val z = steps.reverse.foldLeft("x"):
    case (x, Shuffle.DealWithIncrement(m)) => s"($x * ${m.mulInv(n)})"
    case (x, Shuffle.Cut(m))               => s"($x + $m)"
    case (x, Shuffle.DealIntoNewStack)     => s"(${n - 1} - $x)"
  println(z)

  val y = steps.reverse.foldLeft(x):
    case (x, Shuffle.DealWithIncrement(m)) => (x * m.mulInv(n)) %% n
    case (x, Shuffle.Cut(m))               => (x + m)           %% n
    case (x, Shuffle.DealIntoNewStack)     => (n - 1 - x)       %% n

  println(s"YES $y")

  9
}

extension (deck: Vector[Int])
  def shuffle(steps: Vector[Shuffle]): Vector[Int] =
    val n = deck.size
    steps
      .foldLeft(deck):
        case (deck, Shuffle.DealIntoNewStack)     => deck.reverse
        case (deck, Shuffle.Cut(m))               => deck.drop(m %% n) ++ deck.take(m %% n)
        case (deck, Shuffle.DealWithIncrement(m)) =>
          val inv = m.mulInv(n).toInt
          (0 until n).map(i => deck((i * inv) % n)).toVector

enum Shuffle:
  case DealIntoNewStack
  case DealWithIncrement(m: Long)
  case Cut(m: Long)

extension (self: String)
  def parse: Vector[Shuffle] = self.linesv.collect:
    case "deal into new stack"          => Shuffle.DealIntoNewStack
    case s"deal with increment ${L(i)}" => Shuffle.DealWithIncrement(i)
    case s"cut ${L(i)}"                 => Shuffle.Cut(i)
