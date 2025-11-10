package org.merlin.aoc
package year2022.day22

import lib.{*, given}

import scala.annotation.tailrec

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
  solve(input, overflow2D)

def part2(input: String): Long =
  solve(input, if input == sample then overflow3DSample else overflow3DActual)

def solve(input: String, overflow: (Vector[String], Vec2, Vec2) => (Vec2, Vec2)): Long =
  val (map, instructions) = input.parse
  val start               = map.gridIterator.collectFirst:
    case (loc, '.') => loc
  val (pos, dir)          = instructions.foldLeft(start.get -> East):
    case ((pos, dir), I(num)) =>
      @tailrec def move(map: Vector[String], pos: Vec2, dir: Vec2, count: Int): (Vec2, Vec2) =
        if count == 0 then pos -> dir
        else
          map.get(pos + dir) match
            case Some('#') => pos -> dir
            case Some('.') => move(map, pos + dir, dir, count - 1)
            case _         =>
              val (pos2, dir2) = overflow(map, pos, dir)
              if map.is(pos2, '#') then pos -> dir else move(map, pos2, dir2, count - 1)
      move(map, pos, dir, num)
    case ((pos, dir), "R")    => (pos, dir.cw)
    case ((pos, dir), _)      => (pos, dir.ccw)
  4 * (1 + pos.x) + 1000 * (1 + pos.y) + Score(dir)

val Score = Map(East -> 0, South -> 1, West -> 2, North -> 3)

def overflow2D(map: Vector[String], pos: Vec2, dir: Vec2): (Vec2, Vec2) =
  Iterator
    .iteropt(pos): pos =>
      val nxt = pos + dir.negate
      map.get(nxt).filterNot(_.isSpaceChar).as(nxt)
    .last() -> dir

def overflow3DSample(map: Vector[String], pos: Vec2, dir: Vec2): (Vec2, Vec2) =
  val side = map.length / 3
  if dir.x == 0 then
    (dir.y, pos.x.divMod(side)) match
      case (-1, (0, x)) => ((side * 3 - 1 - x, 0), South)            // 2 -> 1
      case (-1, (1, x)) => ((side * 2, x), East)                     // 3 -> 1
      case (-1, (2, x)) => ((side - 1 - x, side), South)             // 1 -> 2
      case (-1, (_, x)) => ((side * 3 - 1, side * 2 - 1 - x), West)  // 6 -> 4
      case (1, (0, x))  => ((side * 3 - 1 - x, side * 3 - 1), North) // 2 -> 5
      case (1, (1, x))  => ((side * 2, side * 3 - 1 - x), East)      // 3 -> 5
      case (1, (2, x))  => ((side - 1 - x, side * 2 - 1), North)     // 5 -> 2
      case (_, (_, x))  => ((0, side * 2 - 1 - x), East)             // 6 -> 2
  else
    (dir.x, pos.y.divMod(side)) match
      case (1, (0, y))  => ((side * 4 - 1, side * 3 - 1 - y), West)  // 1 -> 6
      case (1, (1, y))  => ((side * 4 - 1 - y, side * 2), South)     // 4 -> 6
      case (1, (_, y))  => ((side * 3 - 1, side - 1 - y), West)      // 5 -> 1
      case (-1, (0, y)) => ((side + y, side), South)                 // 1 -> 3
      case (-1, (1, y)) => ((side * 4 - 1 - y, side * 3 - 1), North) // 2 -> 6
      case (_, (_, y))  => ((side * 2 - 1 - y, side * 2 - 1), North) // 5 -> 3

def overflow3DActual(map: Vector[String], pos: Vec2, dir: Vec2): (Vec2, Vec2) =
  val side = map.length / 4
  if dir.x == 0 then
    (dir.y, pos.x.divMod(side)) match
      case (-1, (0, x)) => ((side, side + x), East)
      case (-1, (1, x)) => ((0, side * 3 + x), East)
      case (-1, (_, x)) => ((x, side * 4 - 1), North)
      case (1, (0, x))  => ((side * 2 + x, 0), South)
      case (1, (1, x))  => ((side - 1, side * 3 + x), West)
      case (_, (_, x))  => ((side * 2 - 1, side + x), West)
  else
    (dir.x, pos.y.divMod(side)) match
      case (1, (0, y))  => ((side * 2 - 1, side * 3 - 1 - y), West)
      case (1, (1, y))  => ((side * 2 + y, side - 1), North)
      case (1, (2, y))  => ((side * 3 - 1, side - 1 - y), West)
      case (1, (_, y))  => ((side + y, side * 3 - 1), North)
      case (-1, (0, y)) => ((0, side * 3 - 1 - y), East)
      case (-1, (1, y)) => ((y, side * 2), South)
      case (-1, (2, y)) => ((side, side - 1 - y), East)
      case (_, (_, y))  => ((side + y, 0), South)

val InstructionRE = """R|L|\d+""".r

extension (string: String)
  def parse: (Vector[String], Vector[String]) =
    val (map, instructions) = string.linesv.bichunk
    val width               = map.maxMap(_.length)
    (map.map(_.padTo(width, ' ')), InstructionRE.findAllIn(instructions.head).toVector)
