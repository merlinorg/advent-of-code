package org.merlin.aoc
package year2020.day21

import lib.{*, given}
import lib.memo.*

@main def part1(): Unit =
  println(part1(sample))
  println(part1(actual))

@main def part2(): Unit =
  println(part2(sample))
  println(part2(actual))

val sample: String = load("sample.txt")
val actual: String = load("actual.txt")

def part1(input: String): Long =
  val list      = input.parse
  val allergens = solve(list)
  val safe      = list.flatMap(_._1).toSet -- allergens.keySet
  list.sumMap: (ingredients, _) =>
    ingredients.count(safe)

def part2(input: String): String =
  val allergens = solve(input.parse)
  allergens.toVector.sortBy(_._2).map(_._1).mkString(",")

def solve(list: Vector[(Set[String], Set[String])]): Map[String, String] =
  val allergenMap = list.foldLeft(Map.empty[String, Set[String]]):
    case (map, (ingredients, allergens)) =>
      allergens.foldLeft(map): (map, allergen) =>
        map.updatedWith(allergen): existing =>
          existing.map(_ & ingredients).orElse(Some(ingredients))
  memoizedFind[Map[String, String]](allergenMap -> Map.empty[String, String]):
    case ((EmptyMap, allergens), _)      => Some(allergens)
    case ((candidates, allergens), loop) =>
      val (allergen, foods) = candidates.minBy(_._2.size)
      foods.findMapOpt: food =>
        Option(food)
          .filterNot(allergens.contains)
          .flatMap: food =>
            loop(candidates.view.mapValues(_ - food).toMap - allergen, allergens + (food -> allergen))

extension (self: String)
  def parse: Vector[(Set[String], Set[String])] =
    self.linesv.collect:
      case s"$ingredients (contains $allergens)" =>
        ingredients.words.toSet -> allergens.split(", ").toSet
