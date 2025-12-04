package org.merlin.aoc
package year2019

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Test extends AnyFreeSpec with Matchers:
  "2019" - {
    "day 01" - {
      "part 1" - {
        "sample" in (day01.part1(day01.sample) shouldBe 34241)
        "actual" in (day01.part1(day01.actual) shouldBe 3390830)
      }
      "part 2" - {
        "sample" in (day01.part2(day01.sample) shouldBe 51316)
        "actual" in (day01.part2(day01.actual) shouldBe 5083370)
      }
    }
    "day 02" - {
      "part 1" - {
        "sample" in (day02.part1(day02.sample) shouldBe 100)
        "actual" in (day02.part1(day02.actual) shouldBe 3760627)
      }
      "part 2" - {
        "actual" in (day02.part2(day02.actual) shouldBe 7195)
      }
    }
    "day 03" - {
      "part 1" - {
        "sample" in (day03.part1(day03.sample) shouldBe 135)
        "actual" in (day03.part1(day03.actual) shouldBe 266)
      }
      "part 2" - {
        "sample" in (day03.part2(day03.sample) shouldBe 410)
        "actual" in (day03.part2(day03.actual) shouldBe 19242)
      }
    }
    "day 04" - {
      "part 1" - {
        "actual" in (day04.part1(day04.actual) shouldBe 1019)
      }
      "part 2" - {
        "actual" in (day04.part2(day04.actual) shouldBe 660)
      }
    }
    "day 05" - {
      "part 1" - {
        "actual" in (day05.part1(day05.actual) shouldBe 4887191)
      }
      "part 2" - {
        "actual" in (day05.part2(day05.actual) shouldBe 3419022)
      }
    }
    "day 06" - {
      "part 1" - {
        "sample" in (day06.part1(day06.sample1) shouldBe 42)
        "actual" in (day06.part1(day06.actual) shouldBe 200001)
      }
      "part 2" - {
        "sample" in (day06.part2(day06.sample2) shouldBe 4)
        "actual" in (day06.part2(day06.actual) shouldBe 379)
      }
    }
    "day 07" - {
      "part 1" - {
        "actual" in (day07.part1(day07.actual) shouldBe 21860)
      }
      "part 2" - {
        "actual" in (day07.part2(day07.actual) shouldBe 2645740)
      }
    }
    "day 08" - {
      "part 1" - {
        "sample" in (day08.part1(day08.sample) shouldBe 1)
        "actual" in (day08.part1(day08.actual) shouldBe 1452)
      }
      "part 2" - {
        "sample" in (day08.part2(day08.sample) shouldBe
          """183
            |456""".stripMargin)
        "actual" in (day08.part2(day08.actual) shouldBe
          """1110010010111001111010010
            |1001010010100101000010010
            |1001011110100101110010010
            |1110010010111001000010010
            |1000010010100001000010010
            |1000010010100001111001100""".stripMargin)
      }
    }
    "day 09" - {
      "part 1" - {
        "actual" in (day09.part1(day09.actual) shouldBe 3638931938L)
      }
      "part 2" - {
        "actual" in (day09.part2(day09.actual) shouldBe 86025)
      }
    }
  }
