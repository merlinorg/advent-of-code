package org.merlin.aoc
package year2019

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Test extends AnyFreeSpec with Matchers:
  "2019" - {
    "day 01" - {
      "part 1" in {
        day01.part1(day01.sample) shouldBe 34241
        day01.part1(day01.actual) shouldBe 3390830
      }
      "part 2" in {
        day01.part2(day01.sample) shouldBe 51316
        day01.part2(day01.actual) shouldBe 5083370
      }
    }
    "day 02" - {
      "part 1" in {
        day02.part1(day02.sample) shouldBe 100
        day02.part1(day02.actual) shouldBe 3760627
      }
      "part 2" in {
        day02.part2(day02.actual) shouldBe 7195
      }
    }
    "day 03" - {
      "part 1" in {
        day03.part1(day03.sample) shouldBe 135
        day03.part1(day03.actual) shouldBe 266
      }
      "part 2" in {
        day03.part2(day03.sample) shouldBe 410
        day03.part2(day03.actual) shouldBe 19242
      }
    }
    "day 04" - {
      "part 1" in {
        day04.part1(day04.actual) shouldBe 1019
      }
      "part 2" in {
        day04.part2(day04.actual) shouldBe 660
      }
    }
  }
