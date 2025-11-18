package org.merlin.aoc
package year2023
package day15

import lib.{*, given}

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

private type Lens = (String, Int)

extension (self: String)
  private def hash: Int =
    self.foldLeft(0)((h, c) => ((h + c) * 17) % 256)

extension (self: List[Lens])
  private def remove(label: String): List[Lens] =
    self.filterNot(_._1 == label)

  private def add(label: String, focus: Int): List[Lens] =
    if self.exists(_._1 == label) then self.map(lens => if lens._1 == label then label -> focus else lens)
    else self :+ (label -> focus)

def part1(lines: Vector[String]): Long =
  lines.flatMap(_.split(",")).sumMap(_.hash)

def part2(lines: Vector[String]): Long =
  lines
    .flatMap(_.split(","))
    .foldLeft(Map.empty[Int, List[Lens]]):
      case (boxes, s"$label-")       =>
        boxes.updatedWith(label.hash)(_.map(_.remove(label)))
      case (boxes, s"$label=$focus") =>
        boxes.updatedWith(label.hash)(_.map(_.add(label, focus.toInt)).orElse(Some(List(label -> focus.toInt))))
      case (boxes, _)                => boxes
    .toList
    .sumMap:
      case (box, lenses) =>
        lenses.zipWithIndex.sumMap((lens, index) => (box + 1) * (index + 1) * lens._2)
