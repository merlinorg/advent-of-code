package org.merlin.aoc
package year2025
package day12

import lib.{*, given}

@main def part1(): Unit =
//  println(part1(sample))
  println(part1(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long = {
  val (presents, regions) = input.parse
  regions.count: (x, y, counts) =>
    val area    = x * y
    val regions = (x / 3) * (y / 3)
    val tiles   = counts.zipWithIndex.sumMap: (count, i) =>
      count * presents(i).gridChars.countA('#')
    if tiles > area then false              // trivially impossible
    else if counts.sum <= regions then true // trivially possible
    else canFit(presents, x, y, counts)
}

// a 5 and 2 3s will fill 3x5
// a 2 and 2 0s will fill 3x5
// a 0 and a 1 and a 3 will fill a 3x7

// This is obviously intractable for the actual problems.
def canFit(presents: Vector[Vector[String]], x: Int, y: Int, counts: Vector[Int]): Boolean = {
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
        counts2             = counts.updated(index, counts(index) - 1)
        rot                <- rotations
        i                  <- 0 to x - 3
        j                  <- 0 to y - 3
        translate           = (i, j)
        translated          = rot.map(_ + translate)
        if translated.fornone(set)
        result             <- loop(counts2, set ++ translated)
      yield result
      results.nextOption()

  val result = loop(counts, Set.empty)
  result.foreach(set => println(set.toGrid.mkString("\n")))
  result.isDefined
}

extension (self: String)
  def parse: (Vector[Vector[String]], Vector[(Int, Int, Vector[Int])]) =
    val chunks = self.linesv.chunks
    (
      chunks.init.map(_.tail),
      chunks.last.collect:
        case s"${I(x)}x${I(y)}: $s" => (x, y, s.split(' ').map(_.toInt).toVector)
    )
