package org.merlin.aoc
package year2023
package day13

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

type Area = Vector[Vector[Char]]

private def parseAreas(lines: Vector[String]): Vector[Area] =
  lines.selectSplit(_.nonEmpty).map(_.map(_.toVector))

private def differences(r0: Vector[Char], r1: Vector[Char]): Int =
  r0.zip(r1).count(_ != _)

private def reflections(a: Area, smudges: Int): Seq[Int] =
  for
    row            <- a.indices.tail
    (before, after) = a.splitAt(row)
    if smudges == before.reverse.zip(after).sumMap(differences)
  yield row

def part1(lines: Vector[String]): Long =
  parseAreas(lines).sumMap: area =>
    reflections(area, 0).map(_ * 100).sum + reflections(area.transpose, 0).sum

def part2(lines: Vector[String]): Long =
  parseAreas(lines).sumMap: area =>
    reflections(area, 1).map(_ * 100).sum + reflections(area.transpose, 1).sum

// Part 2 in "6" lines

// def reflection(a: Vector[Vector[Char]]): Option[Int] =
//   a.indices.tail.find: row =>
//     a.take(row).reverse.zip(a.drop(row)).foldMap((r0, r1) => (r0 zip r1).count(_ != _)) == 1
//
// val areas = Source.fromResource("day-13.txt").getLines.toVector.selectSplit(_.nonEmpty).map(_.map(_.toVector))
//
// areas.foldMap: area =>
//   reflection(area).foldMap(_ * 100) + reflection(area.transpose).sum
