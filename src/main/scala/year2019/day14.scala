package org.merlin.aoc
package year2019.day14

import lib.{*, given}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long =
  solve(input.parse, 1L)

def part2(input: String): Long =
  val factory = input.parse
  Iterator
    .iterate(1L -> Trillion): (a, b) =>
      val mid = (a + b + 1) / 2
      if solve(factory, mid) > Trillion then a -> (mid - 1) else mid -> b
    .findMap: (a, b) =>
      Option.when(a == b)(a)

def solve(factory: Factory, fuel: Long): Long =
  val factory2 = factory ++ OreIsOre // help us out
  Iterator
    .iterate((Map("FUEL" -> fuel), Map.empty[String, Long])): (need, spare) =>
      need.foldLeft(Map.empty[String, Long] -> spare):
        case ((need, spare), (key, qty)) =>
          // take what we can from the spares
          val (qty2, spare2)         = spare.consume(key, qty)
          // find what we need to make this
          val (reagents, multiplier) = factory2(key)
          // some reactions produce chunks of output
          val chunks                 = (qty2 + multiplier - 1) / multiplier
          // add in all the new requirements
          val need2                  = reagents.foldLeft(need):
            case (need, (key, qty)) => need.add(key, qty * chunks)
          need2 -> spare2.add(key, chunks * multiplier - qty2)
    .findMap: (need, _) =>
      Option.when(need.keySet == Set("ORE"))(need("ORE"))

type Reagents = Map[String, Long]
type Factory  = Map[String, (Reagents, Long)]

val OreIsOre: Factory = Map("ORE" -> (Map("ORE" -> 1L) -> 1L))

val Trillion = 1_000_000_000_000L

extension (self: Reagents)
  def consume(key: String, amount: Long): (Long, Reagents) =
    val use = self.getOrElse(key, 0L) min amount
    (amount - use) -> self.add(key, -use)

  def add(key: String, amount: Long): Reagents =
    self.updatedWith(key)(_.map(_ + amount).orElse(Some(amount)).filter(_ != 0))

extension (self: String)
  def parse: Factory = self.linesv.collectToMap:
    case s"$in => ${L(qty)} $s" => s -> (in.parse0, qty)

  private def parse0: Reagents = self
    .split(", ")
    .collectToMap:
      case s"${L(n)} $s" => s -> n
