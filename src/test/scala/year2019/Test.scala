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
    "day 11" - {
      "part 1" - {
        "actual" in (day11.part1(day11.actual) shouldBe 1967)
      }
      "part 2" - {
        "actual" in (day11.part2(day11.actual) shouldBe
          """#..#.###..#..#.####..##..####.###..#..#
            |#.#..#..#.#..#.#....#..#....#.#..#.#.#.
            |##...###..#..#.###..#......#..###..##..
            |#.#..#..#.#..#.#....#.##..#...#..#.#.#.
            |#.#..#..#.#..#.#....#..#.#....#..#.#.#.
            |#..#.###...##..####..###.####.###..#..#""".stripMargin)
      }
    }
    "day 12" - {
      "part 1" - {
        "sample" in (day12.part1(day12.sample) shouldBe 183)
        "actual" in (day12.part1(day12.actual) shouldBe 14606)
      }
      "part 2" - {
        "sample" in (day12.part2(day12.sample) shouldBe 2772)
        "actual" in (day12.part2(day12.actual) shouldBe 543673227860472L)
      }
    }
    "day 13" - {
      "part 1" - {
        "actual" in (day13.part1(day13.actual) shouldBe 326)
      }
      "part 2" - {
        "actual" in (day13.part2(day13.actual) shouldBe 15988)
      }
    }
    "day 14" - {
      "part 1" - {
        "sample" in (day14.part1(day14.sample) shouldBe 13312)
        "actual" in (day14.part1(day14.actual) shouldBe 143173)
      }
      "part 2" - {
        "sample" in (day14.part2(day14.sample) shouldBe 82892753L)
        "actual" in (day14.part2(day14.actual) shouldBe 8845261)
      }
    }
    "day 15" - {
      "part 1" - {
        "actual" in (day15.part1(day15.actual) shouldBe 300)
      }
      "part 2" - {
        "actual" in (day15.part2(day15.actual) shouldBe 312)
      }
    }
    "day 16" - {
      "part 1" - {
        "sample" in (day16.part1(day16.sample1) shouldBe 24176176)
        "actual" in (day16.part1(day16.actual) shouldBe 90744714)
      }
      "part 2" - {
        "sample" in (day16.part2(day16.sample2) shouldBe 84462026)
        "actual" in (day16.part2(day16.actual) shouldBe 82994322)
      }
    }
    "day 17" - {
      "part 1" - {
        "actual" in (day17.part1(day17.actual) shouldBe 2804)
      }
      "part 2" - {
        "actual" in (day17.part2(day17.actual) shouldBe 833429)
      }
    }
    "day 18" - {
      "part 1" - {
        "sample" in (day18.part1(day18.sample1) shouldBe 132)
        "actual" in (day18.part1(day18.actual) shouldBe 4770)
      }
      "part 2" - {
        "sample" in (day18.part2(day18.sample2) shouldBe 8)
        "actual" in (day18.part2(day18.actual) shouldBe 1578)
      }
    }
    "day 19" - {
      "part 1" - {
        "actual" in (day19.part1(day19.actual) shouldBe 226)
      }
      "part 2" - {
        "actual" in (day19.part2(day19.actual) shouldBe 7900946)
      }
    }
    "day 20" - {
      "part 1" - {
        "sample" in (day20.part1(day20.sample1) shouldBe 58)
        "actual" in (day20.part1(day20.actual) shouldBe 618)
      }
      "part 2" - {
        "sample" in (day20.part2(day20.sample2) shouldBe 396)
        "actual" in (day20.part2(day20.actual) shouldBe 7152)
      }
    }
    "day 21" - {
      "part 1" - {
        "actual" in (day21.part1(day21.actual) shouldBe 19358416)
      }
      "part 2" - {
        "actual" in (day21.part2(day21.actual) shouldBe 1144641747)
      }
    }
    "day 22" - {
      "part 1" - {
        "sample" in (day22.part1(day22.sample) shouldBe 9)
        "actual" in (day22.part1(day22.actual) shouldBe 3074)
      }
      "part 2" - {
        "actual" in (day22.part2(day22.actual) shouldBe 104073967000066L)
      }
    }
    "day 23" - {
      "part 1" - {
        "actual" in (day23.part1(day23.actual) shouldBe 23213)
      }
      "part 2" - {
        "actual" in (day23.part2(day23.actual) shouldBe 17874)
      }
    }
    "day 24" - {
      "part 1" - {
        "sample" in (day24.part1(day24.sample) shouldBe 2129920)
        "actual" in (day24.part1(day24.actual) shouldBe 18400821)
      }
      "part 2" - {
        "sample" in (day24.part2(day24.sample) shouldBe 99)
        "actual" in (day24.part2(day24.actual) shouldBe 1914)
      }
    }
  }
