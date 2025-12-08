package org.merlin.aoc
package year2021.day16

import lib.{*, given}

import scala.annotation.tailrec

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long =
  def loop(packet: Packet): Long =
    packet.version + packet.children.sumMap(loop)
  loop(input.parse)

def part2(input: String): Long =
  def loop(packet: Packet): Long =
    packet.typeId match
      case 0 => packet.children.sumMap(loop)
      case 1 => packet.children.productMap(loop)
      case 2 => packet.children.minMap(loop)
      case 3 => packet.children.maxMap(loop)
      case 5 => if loop(packet.children(0)) > loop(packet.children(1)) then 1 else 0
      case 6 => if loop(packet.children(0)) < loop(packet.children(1)) then 1 else 0
      case 7 => if loop(packet.children(0)) == loop(packet.children(1)) then 1 else 0
      case _ => packet.literal
  loop(input.parse)

case class Packet(
  version: Int,
  typeId: Int,
  literal: Long,
  children: Vector[Packet],
)

extension (self: String)
  def parse: Packet =
    val bits = self.flatMap: c =>
      val nibble = Integer.parseInt(c.toString, 16).toBinaryString
      ("0" * (4 - nibble.length)) + nibble
    bits.parseAll.head

  private def parseAll: Vector[Packet] =
    @tailrec def loop(bits: String, packets: Vector[Packet]): Vector[Packet] =
      if bits.forall(_ == '0') then packets
      else
        val (tail, packet) = bits.parseOne
        loop(tail, packets :+ packet)
    loop(self, Vector.empty)

  private def parseOne: (String, Packet) =
    val version = self.take(3).parseBinary
    val typeId  = self.slice(3, 6).parseBinary
    val rest    = self.drop(6)
    if typeId == 4 then
      val (tail, literal) = rest.parseLiteral
      (tail, Packet(version, typeId, literal, Vector.empty))
    else if rest.head == '0' then
      val length  = rest.slice(1, 16).parseBinary
      val segment = rest.slice(16, 16 + length)
      val tail    = rest.drop(16 + length)
      (tail, Packet(version, typeId, 0, segment.parseAll))
    else
      @tailrec def loop(bits: String, n: Int, packets: Vector[Packet]): (String, Packet) =
        if n == 0 then (bits, Packet(version, typeId, 0, packets))
        else
          val (tail, packet) = bits.parseOne
          loop(tail, n - 1, packets :+ packet)
      loop(rest.drop(12), rest.slice(1, 12).parseBinary, Vector.empty)

  private def parseLiteral: (String, Long) =
    @tailrec def loop(bits: String, sum: Long): (String, Long) =
      val next = (sum << 4) + bits.slice(1, 5).parseBinary
      if bits.head == '0' then (bits.drop(5), next)
      else loop(bits.drop(5), next)
    loop(self, 0)
