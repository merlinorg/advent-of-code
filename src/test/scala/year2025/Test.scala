package org.merlin.aoc
package year2025

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Test extends AnyFreeSpec with Matchers:
  "2025" - {
    "day 01" - {
      "part 1" - {
        "sample" in (day01.part1(day01.sample) shouldBe 3)
        "actual" in (day01.part1(day01.actual) shouldBe 1100)
      }
      "part 2" - {
        "sample" in (day01.part2(day01.sample) shouldBe 6)
        "actual" in (day01.part2(day01.actual) shouldBe 6358)
      }
      "part 2 alt" - {
        "sample" in (day01.part2Alt(day01.sample) shouldBe 6)
        "actual" in (day01.part2Alt(day01.actual) shouldBe 6358)
      }
    }
    "day 02" - {
      "part 1" - {
        "sample" in (day02.part1(day02.sample) shouldBe 1227775554)
        "actual" in (day02.part1(day02.actual) shouldBe 23534117921L)
      }
      "part 1 alt" - {
        "sample" in (day02.part1Alt(day02.sample) shouldBe 1227775554)
        "actual" in (day02.part1Alt(day02.actual) shouldBe 23534117921L)
      }
      "part 2" - {
        "sample" in (day02.part2(day02.sample) shouldBe 4174379265L)
        "actual" in (day02.part2(day02.actual) shouldBe 31755323497L)
      }
      "part 2 alt" - {
        "sample" in (day02.part2Alt(day02.sample) shouldBe 4174379265L)
        "actual" in (day02.part2Alt(day02.actual) shouldBe 31755323497L)
      }
    }
    "day 03" - {
      "part 1" - {
        "sample" in (day03.part1(day03.sample) shouldBe 357)
        "actual" in (day03.part1(day03.actual) shouldBe 17087)
      }
      "part 2" - {
        "sample" in (day03.part2(day03.sample) shouldBe 3121910778619L)
        "actual" in (day03.part2(day03.actual) shouldBe 169019504359949L)
      }
      "part 2 alt" - {
        "sample" in (day03.part2Alt(day03.sample) shouldBe 3121910778619L)
        "actual" in (day03.part2Alt(day03.actual) shouldBe 169019504359949L)
      }
    }
    "day 04" - {
      "part 1" - {
        "sample" in (day04.part1(day04.sample) shouldBe 13)
        "actual" in (day04.part1(day04.actual) shouldBe 1523)
      }
      "part 2" - {
        "sample" in (day04.part2(day04.sample) shouldBe 43)
        "actual" in (day04.part2(day04.actual) shouldBe 9290)
      }
    }
  }
