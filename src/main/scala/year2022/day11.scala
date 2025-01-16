package org.merlin.aoc
package year2022.day11

import lib.*

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")

val actual: String = load("actual.txt")

def part1(input: String): Long =
  solve(input.parse, 20, _ / 3)

def part2(input: String): Long =
  val monkeys = input.parse
  val lcm     = monkeys.map(v => v(3)).product
  solve(monkeys, 10000, _ % lcm)

def solve(monkeys: Vector[Monkey], iterations: Int, reduce: Long => Long): Long =
  val it = Iterator.iterate(monkeys): monkeys =>
    Iterator
      .iterate(0 -> monkeys): (index, monkeys) =>
        monkeys(index) match
          case (head +: tail, op, amount, div, ifTrue, ifFalse, count) =>
            val amt    = if amount == "old" then head else amount.toLong
            val next   = if op == '+' then reduce(head + amt) else reduce(head * amt)
            val target = if next % div == 0 then ifTrue else ifFalse

            val (items2, op2, amount2, div2, ifTrue2, ifFalse2, count2) = monkeys(target)
            index ->
              monkeys
                .updated(index, (tail, op, amount, div, ifTrue, ifFalse, count + 1))
                .updated(target, (items2 :+ next, op2, amount2, div2, ifTrue2, ifFalse2, count2))
          case _                                                       =>
            index + 1 -> monkeys
      .findMap: (index, monkeys) =>
        (index == monkeys.length).option(monkeys)

  it.nth(iterations).map(v => v(6)).sorted.takeRight(2).product

type Monkey = (Vector[Long], Char, String, Long, Int, Int, Long)

extension (string: String)
  def parse: Vector[Monkey] = string.linesv.chunks.collect:
    case Vector(
          s"Monkey $_:",
          s"  Starting items: ${LV(items)}",
          s"  Operation: new = old ${C(op)} $value",
          s"  Test: divisible by ${L(div)}",
          s"    If true: throw to monkey ${I(ifTrue)}",
          s"    If false: throw to monkey ${I(ifFalse)}"
        ) =>
      (items, op, value, div, ifTrue, ifFalse, 0L)
