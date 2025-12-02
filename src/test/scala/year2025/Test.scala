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
      "part 2 alt" in {
        day01.part2Alt(day01.sample) shouldBe 6
        day01.part2Alt(day01.actual) shouldBe 6358
      }
    }
    "day 02" - {
      "part 1" in {
        day02.part1(day02.sample) shouldBe 1227775554
        day02.part1(day02.actual) shouldBe 23534117921L
      }
      "part 2" in {
        day02.part2(day02.sample) shouldBe 4174379265L
        day02.part2(day02.actual) shouldBe 31755323497L
      }
    }
  }
