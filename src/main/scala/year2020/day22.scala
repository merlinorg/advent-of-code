package org.merlin.aoc
package year2020.day22

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
  Iterator
    .iterate(input.parse): (a, b) =>
      if a.head > b.head then (a.tail :+ a.head :+ b.head, b.tail)
      else (a.tail, b.tail :+ b.head :+ a.head)
    .findCollect:
      case (a, b) if a.isEmpty || b.isEmpty => (a ++ b).score

def part2(input: String): Long =
  def play(hand: Hand): Hand =
    Iterator
      .iterate(hand -> Set.empty[Hand]):
        case ((a, b), visited) =>
          val aWins =
            if visited(a -> b) then true
            else if a.length <= a.head || b.length <= b.head then a.head > b.head
            else play(a.tail.take(a.head) -> b.tail.take(b.head))._1.nonEmpty
          if aWins then (a.tail :+ a.head :+ b.head, b.tail) -> (visited + (a -> b))
          else (a.tail, b.tail :+ b.head :+ a.head) -> (visited + (a -> b))
      .findCollect:
        case ((a, b), _) if a.isEmpty || b.isEmpty => a -> b

  play(input.parse)
    .fold(_ ++ _)
    .score

type Hand = (Vector[Int], Vector[Int])

extension (self: Vector[Int])
  def score: Long =
    self.reverse.zipWithIndex.sumMap: (value, index) =>
      value * (index + 1)

extension (self: String)
  def parse: Hand =
    self.linesv.bichunk.bimap(_.tail.map(_.toInt), _.tail.map(_.toInt))
