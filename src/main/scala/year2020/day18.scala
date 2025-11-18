package org.merlin.aoc
package year2020.day18

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
  input.linesv.sumMap: eqn =>
    eqn.parse.evaluate

def part2(input: String): Long =
  input.linesv.sumMap: eqn =>
    eqn.parse.rewrite.evaluate

enum Token:
  case Num(num: Long)
  case Op(op: String)
  case Eqn(eqn: Vector[Token])

  def evaluate: Long = this match
    case Num(num) => num
    case Op(_)    => ???
    case Eqn(eqn) =>
      eqn.tail.pairs.foldCollect(eqn.head.evaluate):
        case (acc, (Op("+"), value)) => acc + value.evaluate
        case (acc, (Op("*"), value)) => acc * value.evaluate

  def rewrite: Token = this match
    case Eqn(Add(pre, sum, post)) => Eqn((pre :+ Eqn(sum)) ++ post).rewrite
    case Eqn(v)                   => Eqn(v.map(_.rewrite))
    case o                        => o

object Add:
  def unapply(v: Vector[Token]): Option[(Vector[Token], Vector[Token], Vector[Token])] =
    val index = v.indexOf(Token.Op("+"), 2)
    Option.when(index > 0)(v.take(index - 1), v.slice(index - 1, index + 2), v.drop(2 + index))

extension (self: String)
  def parse: Token =
    "[(+*)]|\\d+".r
      .findAllIn(self)
      .foldCollect(List(Token.Eqn(Vector.empty))):
        case (stack, "(")                     => Token.Eqn(Vector.empty) :: stack
        case (eqn :: Token.Eqn(v) :: tail, ")") => Token.Eqn(v :+ eqn) :: tail
        case (Token.Eqn(v) :: tail, I(i))       => Token.Eqn(v :+ Token.Num(i)) :: tail
        case (Token.Eqn(v) :: tail, op)         => Token.Eqn(v :+ Token.Op(op)) :: tail
      .head
