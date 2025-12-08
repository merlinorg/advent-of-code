package org.merlin.aoc
package year2022.day07

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
  val fs = input.parse
  fs.keys.sumMap: dir =>
    val size = fs.df(dir)
    if size <= 100000 then size else 0

def part2(input: String): Long =
  val fs  = input.parse
  val min = 30000000 - (70000000 - fs.df("/"))
  fs.keys.map(fs.df).filter(_ >= min).min

type FS = Map[String, Map[String, Long]]

extension (fs: FS)
  def df(dir: String): Long =
    fs(dir).sumMap: (name, size) =>
      if name.endsWith("/") then df(s"$dir$name") else size

extension (string: String)
  def parse: FS =
    val (_, fs) = string.linesIterator.foldLeft("/" -> Map("/" -> Map.empty[String, Long])):
      case ((_, fs), "$ cd /")              =>
        "/" -> fs
      case ((pwd, fs), "$ cd ..")           =>
        pwd.replaceFirst("""[^/]+/$""", "") -> fs
      case ((pwd, fs), s"$$ cd $dir")       =>
        s"$pwd$dir/" -> fs
      case ((pwd, fs), s"dir $dir")         =>
        pwd -> fs.updatedWith(pwd)(_.map(_ + (s"$dir/" -> 0L))).updated(s"$pwd$dir/", Map.empty)
      case ((pwd, fs), s"${L(size)} $name") =>
        pwd -> fs.updatedWith(pwd)(_.map(_ + (name -> size)))
      case (fs, _)                          => fs
    fs
