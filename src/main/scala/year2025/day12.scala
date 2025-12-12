package org.merlin.aoc
package year2025
package day12

import lib.{*, given}

@main def part1(): Unit =
//  println(part1(sample))
  println(part1(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long =
  val (presents, regions) = input.parse
  regions.count: (x, y, counts) =>
    val blocks = counts.zipWithIndex.sumMap: (count, i) =>
      count * presents(i).gridChars.countA('#')
    if blocks > x * y then false                      // trivially impossible
    else if counts.sum <= (x / 3) * (y / 3) then true // trivially possible
    else canFit(presents, x, y, counts) // intractable

// This is obviously intractable for the actual problems.
def canFit(presents: Vector[Vector[String]], x: Int, y: Int, counts: Vector[Int]): Boolean =
  val transformations = presents.map: p =>
    (Iterator.iterate(p)(_.cw).take(4) ++ Iterator.iterate(p.flipX)(_.cw).take(4))
      .map(_.gridIndices('#'))
      .toSet
      .toVector

  def loop(counts: Vector[Int], set: Set[Vec2]): Option[Set[Vec2]] =
    if counts.forall(_ == 0) then Some(set)
    else
      val results = for
        (rotations, index) <- transformations.zipWithIndex.iterator
        if counts(index) > 0
        rot                <- rotations
        i                  <- 0 to x - 3
        j                  <- 0 to y - 3
        translated          = rot.map(_ + (i, j))
        if translated.fornone(set)
        result             <- loop(counts.updated(index, counts(index) - 1), set ++ translated)
      yield result
      results.nextOption()

  loop(counts, Set.empty) match
    case None      => false
    case Some(set) =>
      println(set.toGrid.mkString("\n"))
      true

extension (self: String)
  def parse: (Vector[Vector[String]], Vector[(Int, Int, Vector[Int])]) =
    val chunks = self.linesv.chunks
    chunks.init.map(_.tail) -> chunks.last.collect:
      case s"${I(x)}x${I(y)}: $s" => (x, y, s.split(' ').map(_.toInt).toVector)
