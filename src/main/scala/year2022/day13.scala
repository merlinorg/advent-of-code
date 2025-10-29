package org.merlin.aoc
package year2022.day13

import year2022.day13.Value.*
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
  input.parse
    .grouped(2)
    .zipWithIndex
    .sumCollect:
      case (Vector(a, b), i) if a < b => i + 1

def part2(input: String): Long =
  val sorted = (input.parse :+ Two :+ Six).sortWith(_ < _)
  (sorted.indexOf(Two) + 1) * (sorted.indexOf(Six) + 1)

val Two = Sequence(Vector(Sequence(Vector(Numeric(2)))))
val Six = Sequence(Vector(Sequence(Vector(Numeric(6)))))

enum Value:
  case Numeric(value: Int)
  case Sequence(values: Vector[Value])

  def <(other: Value): Boolean = cmp(other) < 0

  def cmp(other: Value): Int = (this, other) match
    case (a: Numeric, b: Sequence)                    =>
      Sequence(Vector(a)).cmp(b)
    case (a: Sequence, b: Numeric)                    =>
      a.cmp(Sequence(Vector(b)))
    case (Sequence(a +: aTail), Sequence(b +: bTail)) =>
      a.cmp(b) || Sequence(aTail).cmp(Sequence(bTail))
    case (Numeric(a), Numeric(b))                     => a - b
    case (Sequence(_), Sequence(_ +: _))              => -1
    case (Sequence(_ +: _), Sequence(_))              => 1
    case _                                            => 0

object Value:
  def unapply(s: String): Option[Value] =
    def parse(i: Int): (Int, Value) =
      if s.charAt(i).isDigit then
        val j = s.indexWhere(!_.isDigit, i)
        (j, Numeric(s.substring(i, j).toInt))
      else
        Iterator
          .iterate((i + 1, Vector.empty[Value])): (i, values) =>
            val (j, value) = parse(i)
            (if s(j) == ',' then j + 1 else j, values :+ value)
          .findMap: (i, values) =>
            (s(i) == ']').option((i + 1, Sequence(values)))
    s.nonEmpty.option(parse(0)(1))

extension (string: String)
  def parse: Vector[Value] =
    string.linesv.collect:
      case Value(value) => value
