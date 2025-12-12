package org.merlin.aoc
package year2025

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

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
    "day 05" - {
      "part 1" - {
        "sample" in (day05.part1(day05.sample) shouldBe 3)
        "actual" in (day05.part1(day05.actual) shouldBe 623)
      }
      "part 2" - {
        "sample" in (day05.part2(day05.sample) shouldBe 14)
        "actual" in (day05.part2(day05.actual) shouldBe 353507173555373L)
      }
    }
    "day 06" - {
      "part 1" - {
        "sample" in (day06.part1(day06.sample) shouldBe 4277556)
        "actual" in (day06.part1(day06.actual) shouldBe 6299564383938L)
      }
      "part 2" - {
        "sample" in (day06.part2(day06.sample) shouldBe 3263827)
        "actual" in (day06.part2(day06.actual) shouldBe 11950004808442L)
      }
    }
    "day 07" - {
      "part 1" - {
        "sample" in (day07.part1(day07.sample) shouldBe 21)
        "actual" in (day07.part1(day07.actual) shouldBe 1642)
      }
      "part 2" - {
        "sample" in (day07.part2(day07.sample) shouldBe 40)
        "actual" in (day07.part2(day07.actual) shouldBe 47274292756692L)
      }
    }
    "day 08" - {
      "part 1" - {
        "sample" in (day08.part1(day08.sample) shouldBe 40)
        "actual" in (day08.part1(day08.actual) shouldBe 175500)
      }
      "part 2" - {
        "sample" in (day08.part2(day08.sample) shouldBe 25272)
        "actual" in (day08.part2(day08.actual) shouldBe 6934702555L)
      }
    }
    "day 09" - {
      "part 1" - {
        "sample" in (day09.part1(day09.sample) shouldBe 50)
        "actual" in (day09.part1(day09.actual) shouldBe 4715966250L)
      }
      "part 2" - {
        "sample" in (day09.part2(day09.sample) shouldBe 24)
        "actual" in (day09.part2(day09.actual) shouldBe 1530527040)
      }
    }
    "day 09 alt" - {
      "part 1" - {
        "sample" in (day09alt.part1(day09alt.sample) shouldBe 50)
        "actual" in (day09alt.part1(day09alt.actual) shouldBe 4715966250L)
      }
      "part 2" - {
        "sample" in (day09alt.part2(day09alt.sample) shouldBe 24)
        "actual" in (day09alt.part2(day09alt.actual) shouldBe 1530527040)
      }
    }
    "day 10" - {
      "part 1" - {
        "sample" in (day10.part1(day10.sample) shouldBe 7)
        "actual" in (day10.part1(day10.actual) shouldBe 571)
      }
      "part 2" - {
        "sample" in (day10.part2(day10.sample) shouldBe 33)
        "actual" taggedAs Slow in (day10.part2(day10.actual) shouldBe 20869)
      }
    }
    "day 10 alt" - {
      "part 1" - {
        "sample" in (day10alt.part1(day10alt.sample) shouldBe 7)
        "actual" in (day10alt.part1(day10alt.actual) shouldBe 571)
      }
      "part 2" - {
        "sample" in (day10alt.part2(day10alt.sample) shouldBe 33)
        "actual" in (day10alt.part2(day10alt.actual) shouldBe 20869)
      }
    }
    "day 11" - {
      "part 1" - {
        "sample" in (day11.part1(day11.sample1) shouldBe 5)
        "actual" in (day11.part1(day11.actual) shouldBe 634)
      }
      "part 2" - {
        "sample" in (day11.part2(day11.sample2) shouldBe 2)
        "actual" in (day11.part2(day11.actual) shouldBe 377452269415704L)
      }
    }
    "day 12" - {
      "part 1" - {
        "sample" ignore (day12.part1(day12.sample) shouldBe 2)
        "actual" in (day12.part1(day12.actual) shouldBe 528)
      }
    }
  }
