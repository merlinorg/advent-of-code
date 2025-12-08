package org.merlin.aoc
package year2020.day20

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
  val tiles = input.parse
  tiles
    .flatMap: (id, variants) =>
      val matches = tiles.count: (od, ov) =>
        id != od && ov.exists(_.touches(variants.head).isDefined)
      Option.when(matches == 2)(id)
    .product

def part2(input: String): Long =
  val tiles        = input.parse
  val tile0        = tiles.head._2.head
  val solution     = Iterator
    .iterate(Map(tiles.head._1 -> (Origin, tile0))): map =>
      tiles.foldLeft(map):
        case (map, (id, _)) if map.contains(id) => map
        case (map, (id, variants))              =>
          val position =
            for
              variant     <- variants.view
              (loc, tile) <- map.values
              dir         <- variant.touches(tile)
            yield id -> (loc + dir, variant)
          map ++ position.headOption
    .findFirst(_.size == tiles.size)
    .values
    .toMap
  val (minX, maxX) = solution.keys.rangeMap(_.x)
  val (minY, maxY) = solution.keys.rangeMap(_.y)
  val combined     = for
    y   <- minY to maxY
    row <- 1 until tile0.height - 1
  yield for
    x   <- minX to maxX
    col <- 1 until tile0.width - 1
  yield solution(x -> y)(row)(col)
  val picture      = combined.map(_.mkString).toVector

  val monsterHashes = Monster.variants.toSet
    .flatMap: monster =>
      val hashes = monster.gridIterator.collectToSet:
        case (loc, '#') => loc
      for
        x     <- 0 until picture.width - monster.width
        y     <- 0 until picture.height - monster.height
        offset = hashes.map(_ + (x -> y))
        if offset.forall(picture.is(_, '#'))
      yield offset
    .flatten

  picture.gridChars.countA('#') - monsterHashes.size

val Monster = """                  #.
                |#    ##    ##    ###
                | #  #  #  #  #  #   """.stripMargin.linesv

extension (self: Vector[String])
  def variants: Vector[Vector[String]]             =
    (Iterator.iterate(self)(_.cw).take(4) ++ Iterator.iterate(self.flipX)(_.cw).take(4)).toVector
  def touches(other: Vector[String]): Option[Vec2] =
    if isRightOf(other) then Some(East)
    else if isLeftOf(other) then Some(West)
    else if isAbove(other) then Some(North)
    else if isBelow(other) then Some(South)
    else None
  def isRightOf(other: Vector[String]): Boolean    =
    (0 until self.height).forall(y => self(y)(0) == other(y)(self.width - 1))
  def isLeftOf(other: Vector[String]): Boolean     =
    (0 until self.height).forall(y => self(y)(self.width - 1) == other(y)(0))
  def isBelow(other: Vector[String]): Boolean      =
    (0 until self.width).forall(x => self(0)(x) == other(self.height - 1)(x))
  def isAbove(other: Vector[String]): Boolean      =
    (0 until self.width).forall(x => self(self.height - 1)(x) == other(0)(x))

extension (self: String)
  def parse: Vector[(Long, Vector[Vector[String]])] =
    self.linesv.chunks.collect: chunk =>
      chunk.split1.bimap(_.longs.head, _.variants)
