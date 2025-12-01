package org.merlin.aoc
package year2025

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Test extends AnyFreeSpec with Matchers:
  "2025" - {
    "day 01" - {
      "part 1" in {
        day01.part1(day01.sample) shouldBe 3
        day01.part1(day01.actual) shouldBe 1100
      }
      "part 2" in {
        day01.part2(day01.sample) shouldBe 6
        day01.part2(day01.actual) shouldBe 6358
      }
    }
  }
