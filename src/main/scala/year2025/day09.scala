package org.merlin.aoc
package year2025
package day09

import lib.{*, given}
import lib.map.*

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long = input.parse.allPairs.map(Rect.apply).maxMap(_.area)

def part2(input: String): Long = solve2(input.parse).area

def solve2(vertices: Vector[Vec2]): Rect =
  val allEdges    = (vertices :+ vertices.head).slidingPairs
  val quadrants   = quadrantise(allEdges)
  val maxQuadrant = quadrants.keys.maxMap(_.x)

  vertices.allPairs
    .map(Rect.apply)
    .sortBy(r => -r.area)
    .findFirst: rect =>
      pointsOfInterest(rect, quadrants).forall(isContained(_, quadrants, maxQuadrant))

// This is very much more complex than needed for the given input, which can be
// solved by looking for any edge that intersects with the rectangle interior.
// That would fail, however, for:
//
//   #XX##XX#
//   X..XX..X
//   X..##..X
//   X......X
//   #XXXXXX#

val Quadrant = 1024

// Divide the world into quadrants and return a map of all the edges that pass through each quadrant
def quadrantise(edges: Iterable[Edge]): Map[Vec2, Vector[Edge]] =
  edges.foldLeft(Map.empty[Vec2, Vector[Edge]].withDefaultValue(Vector.empty)):
    case (map, edge) if edge.vertical =>
      val ((x0, y0), (_, y1)) = edge
      ((y0 min y1) / Quadrant to (y0 max y1) / Quadrant).foldLeft(map): (map, y) =>
        map.append((x0 / Quadrant, y), edge)
    case (map, edge)                  =>
      val ((x0, y0), (x1, _)) = edge
      ((x0 min x1) / Quadrant to (x0 max x1) / Quadrant).foldLeft(map): (map, x) =>
        map.append((x, y0 / Quadrant), edge)

// The points we need to test for presence within the polygon are the four corners and the
// points on either side of every intersection of an edge with the rectangle.
def pointsOfInterest(rect: Rect, quadrants: Map[Vec2, Vector[Edge]]): Vector[Vec2] =
  val horizontals = for
    y    <- Seq(rect.tl.y, rect.br.y) // just the two extrema, not all the points
    qx   <- rect.tl.x / Quadrant to rect.br.x / Quadrant
    edge <- quadrants(qx, y / Quadrant)
    if edge.minY <= y && edge.maxY >= y
    x    <- Seq(edge.minX - 1, edge.minX + 1, edge.maxX - 1, edge.maxX + 1)
    if x >= rect.tl.x && x <= rect.br.x
  yield (x, y)
  val verticals   = for
    x    <- Seq(rect.tl.x, rect.br.x)
    qy   <- rect.tl.y / Quadrant to rect.br.y / Quadrant
    edge <- quadrants(x / Quadrant, qy)
    if edge.minX <= x && edge.maxX >= x
    y    <- Seq(edge.minY - 1, edge.minY + 1, edge.maxY - 1, edge.maxY + 1)
    if y >= rect.tl.y && y <= rect.br.y
  yield (x, y)
  rect.corners ++ horizontals ++ verticals

// A point is within the polygon if it is either on an edge, or there is an odd number of
// edges to the right of it (crossing number / even-odd rule).
def isContained(point: Vec2, quadrants: Map[Vec2, Vector[Edge]], maxQuadrant: Int): Boolean =
  val quadrant = point / Quadrant
  quadrants(quadrant).exists(_.contains(point)) ||
  (quadrant.x to maxQuadrant).flatMap(x => quadrants(x -> quadrant.y)).toSet.count(_.rightOf(point)).odd

type Edge = Pair[Vec2]

extension (self: Edge)
  def vertical: Boolean = self(0).x == self(1).x
  def minX: Int         = self(0).x min self(1).x
  def maxX: Int         = self(0).x max self(1).x
  def minY: Int         = self(0).y min self(1).y
  def maxY: Int         = self(0).y max self(1).y

  def contains(v: Vec2): Boolean = v.x >= minX && v.x <= maxX && v.y >= minY && v.y <= maxY
  def rightOf(v: Vec2): Boolean  = v.x < minX && v.y >= minY && v.y <= maxY

case class Rect(tl: Vec2, br: Vec2):
  val tr: Vec2              = (br.x, tl.y)
  val bl: Vec2              = (tl.x, br.y)
  def area: Long            = (1 + br.x - tl.x) *< (1 + br.y - tl.y)
  def corners: Vector[Vec2] = Vector(tl, tr, br, bl)

object Rect:
  def apply(vertices: Pair[Vec2]): Rect = new Rect(vertices(0) min vertices(1), vertices(0) max vertices(1))

extension (self: String) def parse: Vector[Vec2] = self.linesv.flatMap(Vec2.unapply)

@main def writeSvg(): Unit =
  import lib.svg.*
  val rect = solve2(actual.parse)
  Svg(
    Polygon(actual.parse, stroke = "yellow"),
    Polygon(rect.corners, fill = "green")
  ).writeTo("polygon2.svg")
