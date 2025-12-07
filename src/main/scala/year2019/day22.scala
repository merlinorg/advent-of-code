package org.merlin.aoc
package year2019
package day22

import lib.modulus.*
import lib.{*, given}

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long =
  val n = if input.length < 1000 then 10 else 10007
  input.parse
    .shuffle((0 until n).toVector)
    .indexOf(if input.length < 1000 then 6 else 2019)

def part2(input: String): Long =
  test2(input, 10, 10007)

  given BigMod   = BigMod(119315717514047L)
  val steps      = input.parse
  val (mul, add) = steps.linearEq
  val shuffles   = 101741582076661L
  val card       = 2020
  ((mul ** shuffles) * card + add.geometricSum(mul, shuffles)).toLong

def test2(input: String, card: Int, cards: Int): Unit =
  given M: BigMod = BigMod(cards)

  val steps = input.parse
  val deck  = (0 until cards).toVector

  val deck1 = steps.shuffle(deck)

  val calculated = steps.reverse.foldLeft(BigMod(card)):
    case (x, Shuffle.DealWithIncrement(m)) => x * m.modInv
    case (x, Shuffle.Cut(m))               => x + m
    case (x, Shuffle.DealIntoNewStack)     => M - 1 - x

  assert(calculated.toIndex == deck1(card))

  val (mul, add) = steps.linearEq
  val math       = mul * card + add
  assert(math.toIndex == deck1(card))

  val deck2 = steps.shuffle(deck1)

  val math2 = (mul ** 2) * card + add.geometricSum(mul, 2)
  assert(math2.toIndex == deck2(card))
end test2

extension (steps: Vector[Shuffle])
  def shuffle(deck: Vector[Int]): Vector[Int] =
    given BigMod = BigMod(deck.size)
    steps
      .foldLeft(deck):
        case (deck, Shuffle.DealIntoNewStack)     => deck.reverse
        case (deck, Shuffle.Cut(m))               => deck.drop(m.toIndex) ++ deck.take(m.toIndex)
        case (deck, Shuffle.DealWithIncrement(m)) =>
          val inv = m.modInv.toIndex
          deck.indices.map(i => deck((i * inv) % deck.size)).toVector

  /** Reduce to linear equation `shuffled(x) = b * x + a` */
  def linearEq(using M: BigMod): (mul: BigMod, add: BigMod) =
    steps.reverse.foldLeft((mul = BigMod.one, add = BigMod.zero)):
      case ((mul, add), Shuffle.DealWithIncrement(m)) => (mul * m.modInv, add * m.modInv)
      case ((mul, add), Shuffle.Cut(m))               => (mul, add + m)
      case ((mul, add), Shuffle.DealIntoNewStack)     => (-mul, M - 1 - add)

enum Shuffle:
  case DealIntoNewStack
  case DealWithIncrement(m: BigMod)
  case Cut(m: BigMod)

extension (self: String)
  def parse: Vector[Shuffle] = self.linesv.collect:
    case "deal into new stack"               => Shuffle.DealIntoNewStack
    case s"deal with increment ${BigMod(i)}" => Shuffle.DealWithIncrement(i)
    case s"cut ${BigMod(i)}"                 => Shuffle.Cut(i)
