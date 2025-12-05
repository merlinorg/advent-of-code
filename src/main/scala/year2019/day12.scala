package org.merlin.aoc
package year2019.day12

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
  simulate(input.parse)
    .nth(1000)
    .sumMap: (position, velocity) =>
      position.magnitude.toLong * velocity.magnitude

// Compute the x, y, z repeats independently and then calculate the LCM.
// To find the repeats we use Either[Long, Set[Vector[Int]] where left means
// we've found the answer, right is the set of visited X position/velocity
// values of all the moons.
def part2(input: String): Long =
  simulate(input.parse)
    .scanLeft(Seq.fill(3)(Either.right[Long](Set.empty[Vector[Int]]))): (repeats, moons) =>
      (0 to 2).map: i =>
        val hash = moons.flatMap(moon => Seq(moon._1.get(i), moon._2.get(i)))
        repeats(i).flatMap: seen =>
          Either.cond(!seen(hash), seen + hash, seen.size)
    .findMap: repeats =>
      Option.when(repeats.forall(_.isLeft))(repeats.flatMap(_.swap.toSeq).lcm)

def simulate(positions: Vector[Vec3]): Iterator[Vector[Pair[Vec3]]] =
  Iterator
    .iterate(positions.map(moon => moon -> Origin3D)): moons =>
      moons.map: (position, velocity) =>
        val updated = moons.foldLeft(velocity): (velocity, other) =>
          velocity + (other._1 - position).sign
        (position + updated) -> updated

extension (self: String)
  def parse: Vector[Vec3] = self.linesv.collect:
    case s"<x=${I(x)}, y=${I(y)}, z=${I(z)}>" => (x, y, z)
