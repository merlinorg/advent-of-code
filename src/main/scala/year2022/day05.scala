package org.merlin.aoc
package year2022.day05

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

def part1(input: String): String =
  val (board, moves) = input.parse
  val result         = moves.foldLeft(board):
    case (board, (qty, from, to)) =>
      board
        .updated(from - 1, board(from - 1).drop(qty))
        .updated(to - 1, board(from - 1).take(qty).reverse ++ board(to - 1))
  result.map(_.head).mkString

def part2(input: String): String =
  val (board, moves) = input.parse
  val result         = moves.foldLeft(board):
    case (board, (qty, from, to)) =>
      board
        .updated(from - 1, board(from - 1).drop(qty))
        .updated(to - 1, board(from - 1).take(qty) ++ board(to - 1))
  result.map(_.head).mkString

extension (string: String)
  def parse: (Vector[Vector[Char]], Vector[Triplet[Int]]) =
    string.linesv.bichunk.bimap(
      head =>
        val max = head.map(_.length).max
        head.map(_.padTo(max, ' ')).transpose.grouped(4).map(_(1).dropRight(1).dropWhile(_.isSpaceChar)).toVector
      ,
      _.collect:
        case s"move ${I(a)} from ${I(b)} to ${I(c)}" => (a, b, c)
    )
