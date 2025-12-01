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
  }
