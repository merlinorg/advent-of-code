package org.merlin.aoc
package year2022.day17

import lib.{*, given}

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
  solve(input, 2022)

def part2(input: String): Long =
  solve(input, 1000000000000L)

def solve(input: String, count: Long): Long =
  val (loop, delta) = findRepeat(input)
  val height        = rockerator(input).findMap: (_, rockdex, _, height, _) =>
    (rockdex == loop + count % loop).option(height)
  height + delta * (count / loop - 1)

def findRepeat(input: String): (Int, Int) =
  rockerator(input)
    .scanLeft((Map.empty[(Int, Int, Vector[Int]), (Int, Int)], 0, Option.empty[(Int, Int)])):
      case ((cache, priorRock, solution), (stringdex, rockdex, _, height, board)) =>
        if priorRock == rockdex then (cache, rockdex, solution)
        else
          val heights = (0 until 7).toVector.map: x =>
            (height - 1 to 0 by -1).indexWhere(y => board(x -> y))
          val key     = (stringdex % input.length, rockdex % Rocks.length, heights)
          val value   = rockdex -> height
          (cache + (key -> value), rockdex, cache.get(key).map(value - _))
    .findMap(_._3)

def rockerator(input: String): Iterator[(Int, Int, Vec2, Int, Set[Vec2])] =
  Iterator.iterate((0, 0, (2, 3), 0, Set.empty[Vec2])): (stringdex, rockdex, pos, height, board) =>
    val rock  = Rocks(rockdex % Rocks.length)
    val char  = input(stringdex % input.length)
    val dir   = if char == '>' then East else West
    val blown = if hit(rock, pos + dir, board) then pos else pos + dir
    if hit(rock, blown + North, board) then
      val newHeight = height max (pos.y + rock.height)
      val newBoard  = board ++ rock.gridIterator.filter(_._1 == '#').map(_._2 + blown)
      (stringdex + 1, rockdex + 1, (2, newHeight + 3), newHeight, newBoard)
    else (stringdex + 1, rockdex, blown + North, height, board)

def hit(rock: Rock, pos: Vec2, board: Set[Vec2]): Boolean =
  (pos.x < 0) || (pos.x + rock.width > 7) || (pos.y < 0) ||
    rock.gridIterator.exists((c, loc) => c == '#' && board(pos + loc))

extension (string: String) def parse: Iterator[String] = string.linesIterator

type Rock = Vector[String]

val Rocks =
  """####
    |
    |.#.
    |###
    |.#.
    |
    |..#
    |..#
    |###
    |
    |#
    |#
    |#
    |#
    |
    |##
    |##
    |""".stripMargin.linesv.chunks.map(_.reverse)
