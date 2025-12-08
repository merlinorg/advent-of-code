package org.merlin.aoc
package year2022.day20

import lib.{*, given}

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long =
  val numbers  = input.parse
  val shuffled = shuffle(numbers, numbers.indices.toVector)
  groveCoords(numbers, shuffled)

def part2(input: String): Long =
  val numbers  = input.parse.map(n => n * 811589153)
  val shuffled =
    Iterator
      .iterate(numbers.indices.toVector): indices =>
        shuffle(numbers, indices)
      .nth(10)
  groveCoords(numbers, shuffled)

def groveCoords(numbers: Vector[Long], indices: Vector[Int]): Long =
  val zeroIndex = indices(numbers.indexOf(0))
  (1 to 3).sumMap: offset =>
    numbers(indices.indexOf((zeroIndex + offset * 1000) % numbers.length))

def shuffle(numbers: Vector[Long], indices: Vector[Int]): Vector[Int] =
  numbers.zipWithIndex.foldLeft(indices):
    case (indices, (value, index)) =>
      val curIndex = indices(index)
      val newIndex =
        if value > 0 then 1 + (curIndex + value - 1) %% (numbers.length - 1)
        else if value < 0 then (curIndex + value) %% (numbers.length - 1)
        else curIndex
      indices.map: dex =>
        if dex == curIndex then newIndex
        else if newIndex < curIndex && dex < curIndex && dex >= newIndex then dex + 1
        else if newIndex > curIndex && dex > curIndex && dex <= newIndex then dex - 1
        else dex

extension (string: String) def parse: Vector[Long] = string.linesv.map(_.toLong)
