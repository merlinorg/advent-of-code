package org.merlin.aoc
package year2021.day10

import lib.{*, given}

import scala.annotation.tailrec

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long =
  input.linesv.sumMap: line =>
    parse(line).fold(Score1(_), _ => 0)

def part2(input: String): Long =
  @tailrec def score(s: String, t: Long): Long =
    if s.isEmpty then t else score(s.tail, t * 5 + Score2(s.head))

  input.linesv
    .flatMap: line =>
      parse(line).toOption.map(score(_, 0))
    .sorted
    .middle

def parse(line: String): Either[Char, String] =
  line.foldLeft(Either.right[Char]("")):
    case (Right(stack), c) =>
      Close.get(c).fold(if stack.startsWith(c) then Right(stack.tail) else Left(c))(d => Right(s"$d$stack"))
    case (failure, _)      => failure

val Score1 = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
val Score2 = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)
val Close  = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')
