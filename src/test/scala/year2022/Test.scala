package org.merlin.aoc
package year2022

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.{CPU, Slow}

class Test extends AnyFreeSpec with Matchers:
  "2022" - {
    "day 01" - {
      "part 1" in {
        day01.part1(day01.sample) shouldBe 24000
        day01.part1(day01.actual) shouldBe 69281
      }
      "part 2" in {
        day01.part2(day01.sample) shouldBe 45000
        day01.part2(day01.actual) shouldBe 201524
      }
    }

    "day 02" - {
      "part 1" in {
        day02.part1(day02.sample) shouldBe 15
        day02.part1(day02.actual) shouldBe 11063
      }
      "part 2" in {
        day02.part2(day02.sample) shouldBe 12
        day02.part2(day02.actual) shouldBe 10349
      }
    }

    "day 03" - {
      "part 1" in {
        day03.part1(day03.sample) shouldBe 157
        day03.part1(day03.actual) shouldBe 8123
      }
      "part 2" in {
        day03.part2(day03.sample) shouldBe 70
        day03.part2(day03.actual) shouldBe 2620
      }
    }

    "day 04" - {
      "part 1" in {
        day04.part1(day04.sample) shouldBe 2
        day04.part1(day04.actual) shouldBe 413
      }
      "part 2" in {
        day04.part2(day04.sample) shouldBe 4
        day04.part2(day04.actual) shouldBe 806
      }
    }

    "day 05" - {
      "part 1" in {
        day05.part1(day05.sample) shouldBe "CMZ"
        day05.part1(day05.actual) shouldBe "RTGWZTHLD"
      }
      "part 2" in {
        day05.part2(day05.sample) shouldBe "MCD"
        day05.part2(day05.actual) shouldBe "STHGRZZFR"
      }
    }

    "day 06" - {
      "part 1" in {
        day06.part1(day06.sample) shouldBe 7
        day06.part1(day06.actual) shouldBe 1850
      }
      "part 2" in {
        day06.part2(day06.sample) shouldBe 19
        day06.part2(day06.actual) shouldBe 2823
      }
    }

    "day 07" - {
      "part 1" in {
        day07.part1(day07.sample) shouldBe 95437
        day07.part1(day07.actual) shouldBe 1306611
      }
      "part 2" in {
        day07.part2(day07.sample) shouldBe 24933642
        day07.part2(day07.actual) shouldBe 13210366
      }
    }

    "day 08" - {
      "part 1" in {
        day08.part1(day08.sample) shouldBe 21
        day08.part1(day08.actual) shouldBe 1832
      }
      "part 2" in {
        day08.part2(day08.sample) shouldBe 8
        day08.part2(day08.actual) shouldBe 157320
      }
    }

    "day 09" - {
      "part 1" in {
        day09.part1(day09.sample) shouldBe 13
        day09.part1(day09.actual) shouldBe 6011
      }
      "part 2" in {
        day09.part2(day09.sample) shouldBe 1
        day09.part2(day09.sample2) shouldBe 36
        day09.part2(day09.actual) shouldBe 2419
      }
    }

    "day 10" - {
      "part 1" in {
        day10.part1(day10.sample) shouldBe 13140
        day10.part1(day10.actual) shouldBe 15120
      }
      "part 2" in {
        day10.part2(day10.sample) shouldBe
          """
            |##..##..##..##..##..##..##..##..##..##..
            |###...###...###...###...###...###...###.
            |####....####....####....####....####....
            |#####.....#####.....#####.....#####.....
            |######......######......######......###.
            |#######.......#######.......#######.....
            |""".stripMargin.trim
        day10.part2(day10.actual) shouldBe
          """
            |###..#..#.###....##.###..###..#.....##.#
            |#..#.#.#..#..#....#.#..#.#..#.#....#..#.
            |#..#.##...#..#....#.###..#..#.#....#..#.
            |###..#.#..###.....#.#..#.###..#....####.
            |#.#..#.#..#....#..#.#..#.#....#....#..#.
            |#..#.#..#.#.....##..###..#....####.#..#.
            |""".stripMargin.trim
      }
    }

    "day 11" - {
      "part 1" in {
        day11.part1(day11.sample) shouldBe 10605
        day11.part1(day11.actual) shouldBe 110264
      }
      "part 2" in {
        day11.part2(day11.sample) shouldBe 2713310158L
        day11.part2(day11.actual) shouldBe 23612457316L
      }
    }

    "day 12" - {
      "part 1" in {
        day12.part1(day12.sample) shouldBe 31
        day12.part1(day12.actual) shouldBe 352
      }
      "part 2" in {
        day12.part2(day12.sample) shouldBe 29
        day12.part2(day12.actual) shouldBe 345
      }
    }

    "day 13" - {
      "part 1" in {
        day13.part1(day13.sample) shouldBe 13
        day13.part1(day13.actual) shouldBe 4821
      }
      "part 2" in {
        day13.part2(day13.sample) shouldBe 140
        day13.part2(day13.actual) shouldBe 21890
      }
    }

    "day 14" - {
      "part 1" in {
        day14.part1(day14.sample) shouldBe 24
        day14.part1(day14.actual) shouldBe 696
      }
      "part 2" in {
        day14.part2(day14.sample) shouldBe 93
        day14.part2(day14.actual) shouldBe 23610
      }
    }

    "day 15" - {
      "part 1" in {
        day15.part1(day15.sample) shouldBe 26
        day15.part1(day15.actual) shouldBe 4502208
      }
      "part 2" in {
        day15.part2(day15.sample) shouldBe 56000011
        day15.part2(day15.actual) shouldBe 13784551204480L
      }
    }

    "day 16" - {
      "part 1" in {
        day16.part1(day16.sample) shouldBe 1651
        day16.part1(day16.actual) shouldBe 2056
      }
      "part 2" taggedAs Slow in {
        day16.part2(day16.sample) shouldBe 1707
        day16.part2(day16.actual) shouldBe 2513
      }
    }

    "day 17" - {
      "part 1" in {
        day17.part1(day17.sample) shouldBe 3068
        day17.part1(day17.actual) shouldBe 3119
      }
      "part 2" in {
        day17.part2(day17.sample) shouldBe 1514285714288L
        day17.part2(day17.actual) shouldBe 1536994219669L
      }
    }

    "day 18" - {
      "part 1" in {
        day18.part1(day18.sample) shouldBe 64
        day18.part1(day18.actual) shouldBe 4608
      }
      "part 2" in {
        day18.part2(day18.sample) shouldBe 58
        day18.part2(day18.actual) shouldBe 2652
      }
    }

    "day 19" - {
      "part 1" taggedAs Slow in {
        day19.part1(day19.sample) shouldBe 33
        day19.part1(day19.actual) shouldBe 817
      }
      "part 2" taggedAs (Slow, CPU) in { // actually memory, not CPU
        day19.part2(day19.sample) shouldBe 3348
        day19.part2(day19.actual) shouldBe 4216
      }
    }

    "day 20" - {
      "part 1" in {
        day20.part1(day20.sample) shouldBe 3
        day20.part1(day20.actual) shouldBe 4066
      }
      "part 2" in {
        day20.part2(day20.sample) shouldBe 1623178306
        day20.part2(day20.actual) shouldBe 6704537992933L
      }
    }

    "day 21" - {
      "part 1" in {
        day21.part1(day21.sample) shouldBe 152
        day21.part1(day21.actual) shouldBe 379578518396784L
      }
      "part 2" in {
        day21.part2(day21.sample) shouldBe 301
        day21.part2(day21.actual) shouldBe 3353687996514L
      }
    }

    "day 22" - {
      "part 1" in {
        day22.part1(day22.sample) shouldBe 6032
        day22.part1(day22.actual) shouldBe 88226
      }
      "part 2" in {
        day22.part2(day22.sample) shouldBe 5031
        day22.part2(day22.actual) shouldBe 57305
      }
    }

    "day 23" - {
      "part 1" in {
        day23.part1(day23.sample) shouldBe 110
        day23.part1(day23.actual) shouldBe 4070
      }
      "part 2" in {
        day23.part2(day23.sample) shouldBe 20
        day23.part2(day23.actual) shouldBe 881
      }
    }

    "day 24" - {
      "part 1" in {
        day24.part1(day24.sample) shouldBe 18
        day24.part1(day24.actual) shouldBe 281
      }
      "part 2" in {
        day24.part2(day24.sample) shouldBe 54
        day24.part2(day24.actual) shouldBe 807
      }
    }

    "day 25" - {
      "part 1" in {
        day25.part1(day25.sample) shouldBe "2=-1=0"
        day25.part1(day25.actual) shouldBe "2=1-=02-21===-21=200"
      }
    }
  }
