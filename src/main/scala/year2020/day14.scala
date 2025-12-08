package org.merlin.aoc
package year2020.day14

import lib.{*, given}

@main def part1(): Unit =
  println(part1(sample1))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample2))
  println(part2(actual))

val sample1: String = load("sample1.txt")
val sample2: String = load("sample2.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long =
  input.parse
    .foldLeft(Map.empty[Int, Long] -> ""):
      case ((memory, _), Op.Mask(mask))             => memory -> mask
      case ((memory, mask), Op.Mem(address, value)) =>
        val masked = mask.zipWithIndex.foldLeft(value):
          case (value, ('0', shift)) => value & ~(1L << shift)
          case (value, ('1', shift)) => value | (1L << shift)
          case (value, _)            => value
        (memory + (address -> masked)) -> mask
    ._1
    .values
    .sum

def part2(input: String): Long =
  input.parse
    .foldLeft(Map.empty[Long, Long] -> ""):
      case ((memory, _), Op.Mask(mask))             => memory -> mask
      case ((memory, mask), Op.Mem(address, value)) =>
        def loop(index: Int, address: Long): Vector[Long] =
          if index == mask.length then Vector(address)
          else if mask(index) == '0' then loop(index + 1, address)
          else if mask(index) == '1' then loop(index + 1, address | (1L << index))
          else loop(index + 1, address) ++ loop(index + 1, address ^ (1L << index))
        (memory ++ loop(0, address).strengthR(value)) -> mask
    ._1
    .values
    .sum

enum Op:
  case Mask(mask: String)
  case Mem(address: Int, value: Long)

extension (self: String)
  def parse: Vector[Op] =
    self.linesv.collect:
      case s"mask = $str"                      => Op.Mask(str.reverse)
      case s"mem[${I(address)}] = ${L(value)}" => Op.Mem(address, value)
