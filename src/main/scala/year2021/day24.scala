package org.merlin.aoc
package year2021.day24

import lib.{*, given}

@main def part1(): Unit =
  println(part1(actual)._1)

@main def part2(): Unit =
  println(part2(actual)._1)

@main def reduce(): Unit =
  val program = actual.parse
  import ALU.*

  val (_, variables) = program.foldLeft(0 -> Map("w" -> Num(0), "x" -> Num(0), "y" -> Num(0), "z" -> Num(0))):
    case ((model, variables), ("inp", reg, _))     =>
      (model + 1, variables + (reg -> Inp(model)))
    case ((model, variables), ("add", reg, value)) =>
      (model, variables + (reg -> Add(variables(reg), variables.getValue(value))))
    case ((model, variables), ("mul", reg, "0"))   =>
      (model, variables + (reg -> Num(0)))
    case ((model, variables), ("mul", reg, value)) =>
      (model, variables + (reg -> Mul(variables(reg), variables.getValue(value))))
    case ((model, variables), ("div", reg, value)) =>
      (model, variables + (reg -> Div(variables(reg), variables.getValue(value))))
    case ((model, variables), ("mod", reg, value)) =>
      (model, variables + (reg -> Mod(variables(reg), variables.getValue(value))))
    case ((model, variables), ("eql", reg, value)) =>
      (model, variables + (reg -> Eql(variables(reg), variables.getValue(value))))
    case ((model, variables), _)                   =>
      (model, variables)

  val equation0 = variables("z")
  val equation1 = equation0.simplifyAll
  println(equation1)
  println(equation0.size)
  println(equation1.size)

val actual: String = load("actual.txt")

def part1(input: String): (Long, Long) =
  // derived from the rules produced by simplifying the ALU equation
  val solution = 91599994399395L
  solution -> compute(input.parse, solution)

def part2(input: String): (Long, Long) =
  // derived from the rules produced by simplifying the ALU equation
  val solution = 71111591176151L
  solution -> compute(input.parse, solution)

def compute(program: Vector[(String, String, String)], value: Long): Long =
  val (_, variables) = program.foldLeft(value.toString -> Map("w" -> 0L, "x" -> 0L, "y" -> 0L, "z" -> 0L)):
    case ((model, variables), ("inp", reg, _))     =>
      (model.tail, variables + (reg -> model.head.asDigit))
    case ((model, variables), ("add", reg, value)) =>
      (model, variables + (reg -> (variables(reg) + variables.getValue(value))))
    case ((model, variables), ("mul", reg, value)) =>
      (model, variables + (reg -> (variables(reg) * variables.getValue(value))))
    case ((model, variables), ("div", reg, value)) =>
      (model, variables + (reg -> (variables(reg) / variables.getValue(value))))
    case ((model, variables), ("mod", reg, value)) =>
      (model, variables + (reg -> (variables(reg) % variables.getValue(value))))
    case ((model, variables), ("eql", reg, value)) =>
      (model, variables + (reg -> (if variables(reg) == variables.getValue(value) then 1 else 0)))
    case ((model, variables), _)                   =>
      (model, variables)
  variables("z")

enum ALU:
  case Num(value: Long)
  case Inp(index: Int)
  case Add(lhs: ALU, rhs: ALU)
  case Mul(lhs: ALU, rhs: ALU)
  case Div(lhs: ALU, rhs: ALU)
  case Mod(lhs: ALU, rhs: ALU)
  case Eql(lhs: ALU, rhs: ALU)
  case Neq(lhs: ALU, rhs: ALU)

  // Rules:
  //   i3 = i4
  //   i6 = i1 + 8
  //   i9 = i8 + 6
  //   i10 = i7 + 5
  //   i11 = i0 - 6
  //   i13 = i12 - 4

  def simplify: ALU = this match
    // identified by viewing and simplifying the printed equation
    case Neq(Inp(3), Inp(4))                                                            => Num(0) // !!!
    case Neq(Add(Inp(2), Num(4)), Inp(5))                                               => Num(0) // !!!
    case Neq(Add(Inp(8), Num(6)), Inp(9))                                               => Num(0) // !!!
    case Neq(Add(Inp(1), Num(8)), Inp(6))                                               => Num(0) // !!!
    case Neq(Add(Inp(7), Num(5)), Inp(10))                                              => Num(0) // !!!
    case Neq(Add(Inp(0), Num(-6)), Inp(11))                                             => Num(0) // !!!
    case Neq(Add(Inp(12), Num(-4)), Inp(13))                                            => Num(0) // !!!
    // simplification rules
    case Add(Num(a), Num(b))                                                            => Num(a + b)
    case Add(lhs, Num(0))                                                               => lhs.simplify
    case Add(Num(0), rhs)                                                               => rhs.simplify
    case Add(Add(lhs, Num(a)), Num(b))                                                  => Add(lhs.simplify, Num(a + b))
    case Mul(_, Num(0))                                                                 => Num(0)
    case Mul(Num(0), _)                                                                 => Num(0)
    case Mul(lhs, Num(1))                                                               => lhs.simplify
    case Mul(Num(1), rhs)                                                               => rhs.simplify
    case Mul(Num(a), Num(b))                                                            => Num(a * b)
    case Div(Num(0), _)                                                                 => Num(0)
    case Div(lhs, Num(1))                                                               => lhs.simplify
    case Div(Add(Inp(_), Num(a)), Num(b)) if a + 9 < b                                  => Num(0)
    case Mod(Num(0), _)                                                                 => Num(0)
    case Mod(Add(Inp(i), Num(a)), Num(b)) if a + 9 < b                                  => Add(Inp(i), Num(a))
    case Mod(Add(Mul(_, Num(a)), rhs), Num(b)) if a == b                                => rhs.simplify
    case Eql(Inp(_), Num(n)) if n < 1 || n > 9                                          => Num(0)
    case Eql(Num(n), Inp(_)) if n < 1 || n > 9                                          => Num(0)
    case Eql(Num(a), Num(b))                                                            => Num(if a == b then 1 else 0)
    case Eql(Eql(lhs, rhs), Num(0))                                                     => Neq(lhs.simplify, rhs.simplify)
    case Eql(Add(Inp(_), Num(a)), Inp(_)) if a.abs >= 9                                 => Num(0)
    case Neq(Inp(_), Num(n)) if n < 1 || n > 9                                          => Num(1)
    case Neq(Num(n), Inp(_)) if n < 1 || n > 9                                          => Num(1)
    case Neq(Num(a), Num(b))                                                            => Num(if a != b then 1 else 0)
    case Neq(Add(Inp(_), Num(a)), Inp(_)) if a.abs >= 9                                 => Num(1)
    case Div(Add(Mul(lhs, Num(a)), Add(Inp(_), Num(b))), Num(c)) if a == c && b + 9 < a => lhs.simplify
    case Add(lhs, rhs)                                                                  => Add(lhs.simplify, rhs.simplify)
    case Mul(lhs, rhs)                                                                  => Mul(lhs.simplify, rhs.simplify)
    case Div(lhs, rhs)                                                                  => Div(lhs.simplify, rhs.simplify)
    case Mod(lhs, rhs)                                                                  => Mod(lhs.simplify, rhs.simplify)
    case Eql(lhs, rhs)                                                                  => Eql(lhs.simplify, rhs.simplify)
    case Neq(lhs, rhs)                                                                  => Neq(lhs.simplify, rhs.simplify)
    case alu                                                                            => alu

  def size: Int = this match
    case Add(lhs, rhs) => 1 + lhs.size + rhs.size
    case Mul(lhs, rhs) => 1 + lhs.size + rhs.size
    case Div(lhs, rhs) => 1 + lhs.size + rhs.size
    case Mod(lhs, rhs) => 1 + lhs.size + rhs.size
    case Eql(lhs, rhs) => 1 + lhs.size + rhs.size
    case Neq(lhs, rhs) => 1 + lhs.size + rhs.size
    case _             => 1

  def simplifyAll: ALU =
    val simplified = simplify
    if size == simplified.size then simplified else simplified.simplifyAll

  override def toString: String = this match
    case Add(lhs, rhs) => s"($lhs + $rhs)"
    case Mul(lhs, rhs) => s"($lhs * $rhs)"
    case Div(lhs, rhs) => s"($lhs / $rhs)"
    case Mod(lhs, rhs) => s"($lhs % $rhs)"
    case Eql(lhs, rhs) => s"($lhs == $rhs)"
    case Neq(lhs, rhs) => s"($lhs != $rhs)"
    case Num(n)        => s"$n"
    case Inp(i)        => s"i$i"

extension (self: Map[String, Long])
  def getValue(value: String): Long =
    self.getOrElse(value, value.toInt)

extension (self: Map[String, ALU])
  def getValue(value: String): ALU =
    self.getOrElse(value, ALU.Num(value.toInt))

extension (self: String)
  def parse: Vector[(String, String, String)] = self.linesv.collect:
    case s"$cmd $one $two" => (cmd, one, two)
    case s"$cmd $one"      => (cmd, one, "")
