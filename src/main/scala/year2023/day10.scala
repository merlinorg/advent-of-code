package org.merlin.aoc
package year2023
package day10

import lib.{*, given}
import lib.fp.{*, given}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample2))
  println(part2(actual))

val sample: Vector[String]  = loadv("sample.txt")
val sample2: Vector[String] = loadv("sample2.txt")
val actual: Vector[String]  = loadv("actual.txt")

private type Point = (Int, Int)

private val N = 0  -> -1
private val E = 1  -> 0
private val S = 0  -> 1
private val W = -1 -> 0

private val connections = Map[Char, List[Point]](
  'S' -> List(N, E, S, W),
  '|' -> List(N, S),
  '-' -> List(W, E),
  'L' -> List(N, E),
  'J' -> List(W, N),
  '7' -> List(S, W),
  'F' -> List(E, S),
).withDefaultValue(Nil)

private def charAt(p: Point, lines: Vector[String]): Char =
  lines.unapply(p._2).flatMap(_.unapply(p._1)) | ' '

private def longestLoop(lines: Vector[String]): List[Point] =
  val sy = lines.indexWhere(_.contains('S'))
  val sx = lines(sy).indexOf('S')
  val s  = (sx, sy)

  val paths = Iterator.iterate(List(s)): path =>
    val pos :: tail = path: @unchecked
    val nxts        = for
      fwd <- connections(charAt(pos, lines))
      nxt  = fwd |+| pos
      rev <- connections(charAt(nxt, lines))
      if pos == (rev |+| nxt) && !tail.headOption.contains(nxt)
    yield nxt
    nxts.head :: path

  paths.drop(1).find(_.head == s).get
end longestLoop

def part1(lines: Vector[String]): Long =
  longestLoop(lines).length / 2

private val walls = connections
  .collect:
    case (chr, dirs) if chr != 'S' && dirs.contains(N) => chr
  .toSet

def part2(lines: Vector[String]): Long =
  val w    = lines.head.length
  val path = longestLoop(lines)
  val els  = path.toSet
  val s    = path.head

  val sWall    = connections.exists:
    case (chr, dirs) =>
      walls(chr) && dirs
        .map(_ |+| s)
        .forall(p => p == path.reverse.tail.head || p == path.tail.head)
  val allWalls = if sWall then walls + 'S' else walls

  val insides = for
    y           <- lines.indices
    (p, inside) <- (0 until w).scanLeft(-1 -> -1 -> false):
                     case ((_, inside), x) =>
                       val p      = x -> y
                       val change = els(p) && allWalls(charAt(p, lines))
                       p -> (inside ^ change)
    if inside && !els.contains(p)
  yield p

  insides.length
