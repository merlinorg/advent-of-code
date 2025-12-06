package org.merlin.aoc
package year2019
package day17

import lib.{*, given}
import lib.queue.*

@main
def part1(): Unit =
  println(part1(actual))

@main
def part2(): Unit =
  println(part2(actual))

val actual: String = load("actual.txt")

def part1(input: String): Long =
  val grid = render(input)
  grid
    .gridIndices('#')
    .filter: loc =>
      loc.neighbours.count(n => grid.is(n, '#')) == 4
    .sumMap(_ * _)

def part2(input: String): Long =
  val grid                 = render(input)
  val (routine, functions) = search(grid)
  val logic                =
    s"""${routine.map(i => ('A' + i).toChar).mkString(",")}
       |${functions.map(_.flatMap(_.productIterator).mkString(",")).mkString("\n")}
       |n
       |""".stripMargin.toVector.map(_.toLong)
  Iterator.unfold(Computer(input, logic, Map(0L -> 2L)))(_.runIO).last()

def search(grid: Vector[String]): (Routine, Vector[Function]) =
  val start = grid.gridIndex('^')
  val total = grid.gridIndices('#').size

  val initialStates = for
    a <- "LR"
    b <- "LR"
    c <- "LR"
  yield (Vector(0), Vector(Vector(a -> 1), Vector(b -> 1), Vector(c -> 1)), 0)

  PriorityQueue.unfold(initialStates*)(using Priority.most(_._3)): (routine, functions, _) =>
    evaluate(grid, start, routine, functions) match
      case Some(n) if n == total => Right(routine -> functions)
      case Some(n)               => Left(nextStates(routine, functions).map(_ :* n))
      case None                  => Left(Vector.empty)

// Evaluate a routine+functions and return the number of places visited
def evaluate(grid: Vector[String], start: Vec2, routine: Routine, functions: Vector[Function]): Option[Int] =
  routine
    .foldLeft(Option((start, North, Set.empty[Vec2]))): (stateOpt, fn) =>
      stateOpt.flatMap: state =>
        functions(fn).foldLeft(Option(state)):
          case (stateOpt, (turn, len)) =>
            stateOpt.flatMap: (loc, dir, visited) =>
              val dir2   = if turn == 'R' then dir.cw else dir.ccw
              val loc2   = loc + dir2 * len
              val places = (loc + dir2) to loc2
              Option.when(places.forall(grid.is(_, '#'))):
                (loc2, dir2, visited ++ places)
    .map(_._3.size)

def nextStates(routine: Routine, functions: Vector[Function]): Vector[(Routine, Vector[Function])] =
  val newRoutines  =
    Vector(0, 1, 2).map(r => (routine :+ r, functions)).filter((r, _) => r.size <= MaxFunctionCalls)
  val newFunctions = permuteFunction(functions(routine.last))
    .map(function => (routine, functions.updated(routine.last, function)))
  newRoutines ++ newFunctions

def permuteFunction(a: Function): Seq[Function] =
  val moreSteps  = "LR".map(dir => a :+ (dir, 1)).filter(_.size <= MaxFunctionSteps)
  val longerWalk = a.lastOption.filter(_._2 < MaxTravel).map((dir, len) => a.dropRight(1) :+ (dir, len + 1))
  moreSteps ++ longerWalk

val MaxFunctionCalls = 10 // 20 / "C,".length
val MaxFunctionSteps = 5  // 20 / "R,4,".length
val MaxTravel        = 16 // observed

type Routine  = Vector[Int]
type Function = Vector[(Char, Int)]

def render(program: String): Vector[String] =
  Iterator
    .unfold(Computer(program)): computer =>
      computer.runIO
    .map(_.toChar)
    .mkString
    .linesv
