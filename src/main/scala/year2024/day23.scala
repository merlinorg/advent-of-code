package org.merlin.aoc
package year2024
package day23

import lib.{*, given}
import lib.fp.{*, given}

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

def part1(lines: Vector[String]): Long =
  parse(lines).cliques(3).toSet.count(_.exists(_.startsWith("t")))

def part2(lines: Vector[String]): String =
  val network = parse(lines)
  (network.values.map(_.size).max to 3 by -1).findMap: size =>
    network.cliques(size).map(_.toVector.sorted.mkString(",")).nextOption()

type Network = Map[String, Set[String]]

extension (network: Network)
  def connected(subnet: Vector[String]): Boolean = subnet.allPairs.forall(connected.tupled)
  def connected(a: String, b: String): Boolean   = network(a)(b)

  def cliques(size: Int): Iterator[Set[String]] =
    network.valuesIterator.flatMap: subnet =>
      subnet.toVector
        .combinations(size)
        .filter(connected)
        .map(_.toSet)

def parse(lines: Vector[String]): Network =
  lines
    .collect:
      case s"$a-$b" => (a, b)
    .foldMap: (a, b) =>
      Map(a -> Set(a, b), b -> Set(a, b))
