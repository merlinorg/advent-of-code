package org.merlin.aoc
package year2023
package day16

import lib.impl.IO.{*, given}
import lib.legacy.*

@main
def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: Vector[String] = loadv("sample.txt")

val actual: Vector[String] = loadv("actual.txt")

private final case class Photon(x: Int, y: Int, dx: Int, dy: Int): // A certain photon
  def location: (Int, Int) = (x, y)

  def move: Photon = copy(x = x + dx, y = y + dy)

  def redirect(dx: Int, dy: Int): Photon = copy(dx = dx, dy = dy)

  def reflect(chr: Char): List[Photon] = chr match
    case '/'            => List(redirect(-dy, -dx))
    case '\\'           => List(redirect(dy, dx))
    case '|' if dx != 0 => List(redirect(0, -1), redirect(0, 1))
    case '-' if dy != 0 => List(redirect(-1, 0), redirect(1, 0))
    case _              => List(this)

private final case class State(energized: Set[(Int, Int)], seen: Set[Photon], photons: List[Photon]):
  def update(board: Vector[String]): State =
    val next = nextPhotons(board)
    State(energized ++ next.map(_.location), seen ++ photons, next)

  private def nextPhotons(board: Vector[String]): List[Photon] =
    for
      photon    <- photons if !seen.contains(photon)
      moved      = photon.move if moved.x >=< board.length && moved.y >=< board.length
      reflected <- moved.reflect(board(moved.y)(moved.x))
    yield reflected

private def energized(photon0: Photon, board: Vector[String]): Long =
  Iterator
    .iterate(State(Set.empty, Set.empty, List(photon0)))(_.update(board))
    .find(_.photons.isEmpty)
    .cata(_.energized.size, 0)

def part1(board: Vector[String]): Long =
  energized(Photon(-1, 0, 1, 0), board)

def part2(board: Vector[String]): Long =
  board.indices
    .flatMap: i =>
      List(Photon(-1, i, 1, 0), Photon(board.length, i, -1, 0), Photon(i, -1, 0, 1), Photon(i, board.length, 0, -1))
    .map: photon =>
      energized(photon, board)
    .max
