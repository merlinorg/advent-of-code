package org.merlin.aoc
package lib.impl

import scala.io.Source
import scala.quoted.*

object IO:
  def load(name: String)(using SF: SourceFile): String =
    Source.fromResource(s"${SF.path}/$name").mkString

  def loadv(name: String)(using SF: SourceFile): Vector[String] =
    Source.fromResource(s"${SF.path}/$name").mkString.linesIterator.toVector

  implicit inline def instance: SourceFile =
    ${ SourceFile.sourceFile_impl }

private case class SourceFile(file: String):
  def path: String =
    """year\d{4}/day\d{2}""".r // .../src/main/scala/year20xx/dayxx.scala
      .findFirstIn(file)
      .getOrElse(throw Error(s"Path $file does not match regex"))

private object SourceFile:
  def sourceFile_impl(using ctx: Quotes): Expr[SourceFile] =
    val rootPosition = ctx.reflect.Position.ofMacroExpansion
    val file         = Expr(rootPosition.sourceFile.path)
    '{ SourceFile($file) }
