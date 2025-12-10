package org.merlin.aoc
package lib

import impl.Vec2Ops.*
import collection.*
import java.nio.file.{Files, Paths}

object svg:
  extension (self: Vector[Vec2]) def toSvg: Svg = Svg(Vector(Polygon(self, stroke = "red")))

  case class Svg(elements: Vector[SvgEl]):
    override def toString: String =
      val (x, y) = elements
        .map(_.range)
        .foldLeft(elements.head.range): (r, s) =>
          (r(0) minMax s(0), r(1) minMax s(1))

      val box = s"${x(0)} ${y(0)} ${x.difference} ${y.difference}"
      s"""<svg width="640" height="640" viewBox="$box" xmlns="http://www.w3.org/2000/svg">
         |  ${elements.map(_.toSvg(x.difference.toDouble / 100)).mkString("\n")}
         |</svg>""".stripMargin

    def writeTo(file: String): Unit =
      Files.write(Paths.get(file), toString.getBytes("UTF-8"))

  trait SvgEl:
    def range: Pair[Vec2]
    def toSvg(strokeWidth: Double): String

  case class Polygon(points: Vector[Vec2], fill: String = "none", stroke: String = "none") extends SvgEl:
    def range: Pair[Vec2] = (points.rangeMap(_.x), points.rangeMap(_.y))

    def toSvg(strokeWidth: Double): String =
      val coords = points.map((x, y) => s"$x,$y").mkString(" ")
      s"""<polygon points="$coords" style="stroke:$stroke;fill:$fill;stroke-width:$strokeWidth" />"""

  case class Circle(center: Vec2, radius: Double, fill: String = "none", stroke: String = "none") extends SvgEl:
    def range: Pair[Vec2] = (center, center)

    def toSvg(strokeWidth: Double): String =
      s"""<circle cx="${center.x}" cy="${center.y}" r="$radius" style="stroke:$stroke;fill:$fill;stroke-width:$strokeWidth" />"""
