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
  input.parse
    .shuffle((0 until n).toVector)
    .indexOf(if input.length < 1000 then 6 else 2019)

def part2(input: String): Long = {
  test(input, 10007L, 10)

  val n          = 119315717514047L
  val steps      = input.parse
  val (mul, add) = steps.reduce(n)
  val nth        = 101741582076661L // <- big, prime
  val x          = 2020
  (powMod(mul, nth, n) * x + geoSum(add, mul, nth, n)) %% n
}

// 2097400128037 too low
// 108045503604693 too high

def test(input: String, n: Long, x: Int): Unit =
  val steps = input.parse
  val deck  = (0 until n.toInt).toVector

  val deck1 = steps.shuffle(deck)

  val calculated = steps.reverse.foldLeft(x.toLong):
    case (x, Shuffle.DealWithIncrement(m)) => (x * m.mulInv(n)) %% n
    case (x, Shuffle.Cut(m))               => (x + m)           %% n
    case (x, Shuffle.DealIntoNewStack)     => (n - 1 - x)       %% n

  assert(calculated == deck1(x))

  val (mul, add) = steps.reduce(n)
  val mathed     = (mul * x + add) %% n
  assert(mathed == deck1(x))

  val deck2 = steps.shuffle(deck1)

  val nth     = 2
  val mathed2 = (powMod(mul, nth, n) * x + geoSum(add, mul, nth, n)) %% n
  assert(mathed2 == deck2(x))
end test

def powMod(b: Long, n: Long, mod: Long): Long =
  b.big.modPow(n, mod).toLong

/** Geometric series `a + a.b + a.b^2 + ... + a.b^(n-1) = a * (1 - b^n) / (1 - b)` */
def geoSum1(a: Long, b: Long, n: Int, mod: Long): Long =
  (a.big * (1.big - b.big.pow(n)) / (1.big - b.big)).mod(mod).toLong

// O(log(n))
def geoSum(a: Long, b: Long, n: Long, mod: Long): Long = {
  var T     = 1.big
  var e     = b.big % mod
  var total = 0.big
  var nn    = n
  while nn > 0 do
    if nn             % 2 == 1 then total = (e * total + T) % mod
    T = ((e + 1) * T) % mod
    e = (e * e)       % mod
    nn = nn / 2
  ((a * total) % mod).toLong
}

extension (steps: Vector[Shuffle])
  def shuffle(deck: Vector[Int]): Vector[Int] =
    val n = deck.size
    steps
      .foldLeft(deck):
        case (deck, Shuffle.DealIntoNewStack)     => deck.reverse
        case (deck, Shuffle.Cut(m))               => deck.drop(m %% n) ++ deck.take(m %% n)
        case (deck, Shuffle.DealWithIncrement(m)) =>
          val inv = m.mulInv(n).toInt
          (0 until n).map(i => deck((i * inv) % n)).toVector

  /** Reduce to the form `shuffled(x) = b * x + a` */
  def reduce(n: Long): (mul: Long, add: Long) =
    steps.reverse.foldLeft((mul = 1L, add = 0L)):
      case ((mul, add), Shuffle.DealWithIncrement(m)) => ((mul * m.mulInv(n)) %% n, (add * m.mulInv(n)) %% n)
      case ((mul, add), Shuffle.Cut(m))               => (mul, (add + m) %% n)
      case ((mul, add), Shuffle.DealIntoNewStack)     => (-mul %% n, (n - 1 - add) %% n)

enum Shuffle:
  case DealIntoNewStack
  case DealWithIncrement(m: Long)
  case Cut(m: Long)

extension (self: String)
  def parse: Vector[Shuffle] = self.linesv.collect:
    case "deal into new stack"          => Shuffle.DealIntoNewStack
    case s"deal with increment ${L(i)}" => Shuffle.DealWithIncrement(i)
    case s"cut ${L(i)}"                 => Shuffle.Cut(i)
