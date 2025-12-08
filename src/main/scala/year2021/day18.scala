package org.merlin.aoc
package year2021.day18

import lib.{*, given}

import scala.annotation.tailrec
import scala.util.matching.Regex

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long =
  val lines = input.linesv
  lines.tail
    .foldLeft(lines.head): (acc, line) =>
      reduce(s"[$acc,$line]")
    .snail
    .magnitude

def part2(input: String): Long =
  input.linesv
    .combinations(2)
    .flatMap: v =>
      Seq(reduce(v.mkString("[", ",", "]")).snail.magnitude, reduce(v.reverse.mkString("[", ",", "]")).snail.magnitude)
    .max

@tailrec def reduce(str: String): String =
  findExplode(str) match
    case Some(explode) => reduce(doExplode(str, explode))
    case None          =>
      SplitRE.findFirstMatchIn(str) match
        case None    => str
        case Some(m) => reduce(doSplit(str, m))

def findExplode(str: String): Option[Int] =
  @tailrec def loop(index: Int, count: Int): Option[Int] =
    if index == str.length then None
    else
      str(index) match
        case '[' if count == 4 => Some(index)
        case '['               => loop(index + 1, count + 1)
        case ']'               => loop(index + 1, count - 1)
        case _                 => loop(index + 1, count)
  loop(0, 0)

def doExplode(str: String, explode: Int): String =
  val comma  = str.indexOf(',', explode)
  val digit0 = str.slice(1 + explode, comma).toInt
  val pre    = "^(.*?)(\\d+)(\\D*)$".r.replaceAllIn(
    str.take(explode),
    m => s"${m.group(1)}${m.group(2).toInt + digit0}${m.group(3)}"
  )
  val close  = str.indexOf(']', comma)
  val digit1 = str.slice(1 + comma, close).toInt
  val post   = "^(\\D*)(\\d+)(.*)$".r.replaceAllIn(
    str.drop(1 + close),
    m => s"${m.group(1)}${m.group(2).toInt + digit1}${m.group(3)}"
  )
  s"${pre}0$post"

val SplitRE = "\\d\\d+".r

def doSplit(str: String, m: Regex.Match): String =
  val number = m.matched.toInt
  val pre    = str.take(m.start)
  val post   = str.drop(m.end)
  s"$pre[${number / 2},${(number + 1) / 2}]$post"

enum Snail:
  def magnitude: Long = this match
    case Integer(n)  => n
    case Tuple(l, r) => 3 * l.magnitude + 2 * r.magnitude

  case Integer(number: Int)
  case Tuple(left: Snail, right: Snail)

object Snail:
  def parse(s: String): (Snail, String) =
    if s.head.isDigit then
      val (num, tail) = s.span(_.isDigit)
      (Snail.Integer(num.toInt), tail)
    else
      val (left, tail0)  = parse(s.tail)
      val (right, tail1) = parse(tail0.tail)
      (Snail.Tuple(left, right), tail1.tail)

extension (self: String) def snail: Snail = Snail.parse(self)._1
