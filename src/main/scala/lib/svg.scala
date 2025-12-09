package org.merlin.aoc
package lib

import impl.Vec2Ops.*
import collection.*
import java.nio.file.{Files, Paths}

object svg:
  extension (self: Vector[Vec2]) def toSvg: Svg = Svg(Polygon(self, stroke = "red"))

  case class Svg(polygons: Polygon*):
    override def toString: String =
      val (x, y) = polygons
        .map(_.range)
        .foldLeft(polygons.head.range): (r, s) =>
          (r(0) minMax s(0), r(1) minMax s(1))

      val box = s"${x(0)} ${y(0)} ${x.difference} ${y.difference}"
      s"""<svg width="640" height="640" viewBox="$box" xmlns="http://www.w3.org/2000/svg">
         |  ${polygons.map(_.toSvg(x.difference / 1000)).mkString("\n")}
         |</svg>""".stripMargin

    def writeTo(file: String): Unit =
      Files.write(Paths.get(file), toString.getBytes("UTF-8"))

  case class Polygon(points: Vector[Vec2], fill: String = "none", stroke: String = "none"):
    def range: Pair[Vec2] = (points.rangeMap(_.x), points.rangeMap(_.y))

    def toSvg(strokeWidth: Int): String =
      val coords = points.map((x, y) => s"$x,$y").mkString(" ")
      s"""<polygon points="$coords" style="stroke:$stroke;fill:$fill;stroke-width:$strokeWidth" />"""
