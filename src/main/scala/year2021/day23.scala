package org.merlin.aoc
package year2021.day23

import lib.{*, given}
import scala.collection.mutable

@main
def part1(): Unit =
  println(part1(sample1))
  println(part1(actual1))

@main
def part2(): Unit =
  println(part2(sample2))
  println(part2(actual2))

val sample1: String = load("sample1.txt")
val sample2: String = load("sample2.txt")

val actual1: String = load("actual1.txt")
val actual2: String = load("actual2.txt")

def part1(input: String): Long = solve(input, 3)

def part2(input: String): Long = solve(input, 5)

def solve(input: String, bottom: Int): Long =
  given Ordering[(Long, Amphipods)] = Ordering.by(t => -t._1)
  val queue                         = mutable.PriorityQueue(0L -> input.parse)
  while !queue.head._2.complete do
    val (cost, amphipods) = queue.dequeue()
    goodMove(cost, amphipods, bottom) match
      case Some(next) =>
        queue.enqueue(next)
      case None       =>
        val nexts = for
          (loc, char)  <- amphipods.toSeq
          (spend, dst) <- allMoves(char, loc, amphipods, bottom)
        yield (cost + spend) -> (amphipods - loc + (dst -> char))
        queue.enqueue(nexts*)
  queue.head._1

def goodMove(cost: Long, amphipods: Amphipods, bottom: Int): Option[(Long, Amphipods)] =
  val good = amphipods.iterator.filter: (loc, char) =>
    loc.x != char.room &&
      (loc.y == 1 || amphipods.hasNot((loc.x, loc.y - 1) to (loc.x, 1))) &&        // can move up
      amphipods.hasNot((loc.x + (char.room - loc.x).sign, 1) to (char.room, 1)) && // can move over
      amphipods.allAre((char.room, 2) to (char.room, bottom), char) // can move in
  Option.when(good.nonEmpty):
    good.foldLeft(cost -> amphipods):
      case ((cost, amphipods), (loc, char)) =>
        val dst   = (char.room, (bottom to 2 by -1).findFirst(y => !amphipods.contains(char.room, y)))
        val entry = (char.room, 1)
        val spend = ((loc |-| entry) + (entry |-| dst)) * char.cost
        (cost + spend) -> (amphipods - loc + (dst -> char))

def allMoves(char: Char, loc: Vec2, amphipods: Amphipods, bottom: Int): Seq[(Long, Vec2)] =
  if loc.y == 1 ||                                   // waiting in hall
  amphipods.has((loc.x, loc.y - 1) to (loc.x, 1)) || // blocked in room
  loc.x == char.room && amphipods.allAre(loc to (loc.x, bottom), char) // done
  then Nil
  else
    for
      x  <- Halls
      dst = (x, 1)
      if amphipods.hasNot((loc.x, 1) to dst)
    yield (loc |-| dst) * char.cost -> dst

val Halls = Seq(1, 2, 4, 6, 8, 10, 11)

extension (self: Amphipods)
  def complete: Boolean                              =
    self.forall: (loc, char) =>
      loc.y > 1 && loc.x == char.room
  def has(it: Iterator[Vec2]): Boolean               =
    it.exists(self.contains)
  def hasNot(it: Iterator[Vec2]): Boolean            =
    it.forall(s => !self.contains(s))
  def allAre(it: Iterator[Vec2], char: Int): Boolean =
    it.forall(l => self.get(l).forall(_ == char))

extension (self: Char)
  def room: Int  = 3 + (self - 'A') * 2
  def cost: Long = 10 ** (self - 'A')

type Amphipods = Map[Vec2, Char]

extension (self: String)
  def parse: Amphipods =
    self.linesv.gridIterator.filter(_._2.isLetter).toMap
