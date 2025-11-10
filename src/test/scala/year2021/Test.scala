package org.merlin.aoc
package year2021

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

class Test extends AnyFreeSpec with Matchers:
  "2021" - {
    "day 01" - {
      "part 1" in {
        day01.part1(day01.sample) shouldBe 7
        day01.part1(day01.actual) shouldBe 1754
      }
      "part 2" in {
        day01.part2(day01.sample) shouldBe 5
        day01.part2(day01.actual) shouldBe 1789
      }
    }
    "day 02" - {
      "part 1" in {
        day02.part1(day02.sample) shouldBe 150
        day02.part1(day02.actual) shouldBe 2027977
      }
      "part 2" in {
        day02.part2(day02.sample) shouldBe 900
        day02.part2(day02.actual) shouldBe 1903644897
      }
    }
    "day 03" - {
      "part 1" in {
        day03.part1(day03.sample) shouldBe 198
        day03.part1(day03.actual) shouldBe 3633500
      }
      "part 2" in {
        day03.part2(day03.sample) shouldBe 230
        day03.part2(day03.actual) shouldBe 4550283
      }
    }
    "day 04" - {
      "part 1" in {
        day04.part1(day04.sample) shouldBe 4512
        day04.part1(day04.actual) shouldBe 8442
      }
      "part 2" in {
        day04.part2(day04.sample) shouldBe 1924
        day04.part2(day04.actual) shouldBe 4590
      }
    }
    "day 05" - {
      "part 1" in {
        day05.part1(day05.sample) shouldBe 5
        day05.part1(day05.actual) shouldBe 4873
      }
      "part 2" in {
        day05.part2(day05.sample) shouldBe 12
        day05.part2(day05.actual) shouldBe 19472
      }
    }
    "day 06" - {
      "part 1" in {
        day06.part1(day06.sample) shouldBe 5934
        day06.part1(day06.actual) shouldBe 372984
      }
      "part 2" in {
        day06.part2(day06.sample) shouldBe 26984457539L
        day06.part2(day06.actual) shouldBe 1681503251694L
      }
    }
    "day 07" - {
      "part 1" in {
        day07.part1(day07.sample) shouldBe 37
        day07.part1(day07.actual) shouldBe 352997
      }
      "part 2" in {
        day07.part2(day07.sample) shouldBe 168
        day07.part2(day07.actual) shouldBe 101571302
      }
    }
    "day 08" - {
      "part 1" in {
        day08.part1(day08.sample) shouldBe 26
        day08.part1(day08.actual) shouldBe 554
      }
      "part 2" in {
        day08.part2(day08.sample) shouldBe 61229
        day08.part2(day08.actual) shouldBe 990964
      }
    }
    "day 09" - {
      "part 1" in {
        day09.part1(day09.sample) shouldBe 15
        day09.part1(day09.actual) shouldBe 603
      }
      "part 2" in {
        day09.part2(day09.sample) shouldBe 1134
        day09.part2(day09.actual) shouldBe 786780
      }
    }
    "day 10" - {
      "part 1" in {
        day10.part1(day10.sample) shouldBe 26397
        day10.part1(day10.actual) shouldBe 367059
      }
      "part 2" in {
        day10.part2(day10.sample) shouldBe 288957
        day10.part2(day10.actual) shouldBe 1952146692
      }
    }
    "day 11" - {
      "part 1" in {
        day11.part1(day11.sample) shouldBe 1656
        day11.part1(day11.actual) shouldBe 1688
      }
      "part 2" in {
        day11.part2(day11.sample) shouldBe 195
        day11.part2(day11.actual) shouldBe 403
      }
    }
    "day 12" - {
      "part 1" in {
        day12.part1(day12.sample) shouldBe 226
        day12.part1(day12.actual) shouldBe 4773
      }
      "part 2" in {
        day12.part2(day12.sample) shouldBe 3509
        day12.part2(day12.actual) shouldBe 116985
      }
    }
    "day 13" - {
      "part 1" in {
        day13.part1(day13.sample) shouldBe 17
        day13.part1(day13.actual) shouldBe 724
      }
      "part 2" in {
        day13.part2(day13.sample) shouldBe
          """#####
            |#...#
            |#...#
            |#...#
            |#####""".stripMargin
        day13.part2(day13.actual) shouldBe
          """.##..###....##.###..####.###..#..#.#...
            |#..#.#..#....#.#..#.#....#..#.#..#.#...
            |#....#..#....#.###..###..#..#.#..#.#...
            |#....###.....#.#..#.#....###..#..#.#...
            |#..#.#....#..#.#..#.#....#.#..#..#.#...
            |.##..#.....##..###..####.#..#..##..####""".stripMargin
      }
    }
    "day 14" - {
      "part 1" in {
        day14.part1(day14.sample) shouldBe 1588
        day14.part1(day14.actual) shouldBe 2345
      }
      "part 2" in {
        day14.part2(day14.sample) shouldBe 2188189693529L
        day14.part2(day14.actual) shouldBe 2432786807053L
      }
    }
    "day 15" - {
      "part 1" in {
        day15.part1(day15.sample) shouldBe 40
        day15.part1(day15.actual) shouldBe 707
      }
      "part 2" in {
        day15.part2(day15.sample) shouldBe 315
        day15.part2(day15.actual) shouldBe 2942
      }
    }
    "day 16" - {
      "part 1" in {
        day16.part1(day16.sample) shouldBe 31
        day16.part1(day16.actual) shouldBe 974
      }
      "part 2" in {
        day16.part2(day16.sample) shouldBe 54
        day16.part2(day16.actual) shouldBe 180616437720L
      }
    }
    "day 17" - {
      "part 1" in {
        day17.part1(day17.sample) shouldBe 45
        day17.part1(day17.actual) shouldBe 6903
      }
      "part 2" in {
        day17.part2(day17.sample) shouldBe 112
        day17.part2(day17.actual) shouldBe 2351
      }
    }
    "day 18" - {
      "part 1" in {
        day18.part1(day18.sample) shouldBe 4140
        day18.part1(day18.actual) shouldBe 4132
      }
      "part 2" in {
        day18.part2(day18.sample) shouldBe 3993
        day18.part2(day18.actual) shouldBe 4685
      }
    }
    "day 19" - {
      "part 1" in {
        day19.part1(day19.sample) shouldBe 79
        day19.part1(day19.actual) shouldBe 465
      }
      "part 2" in {
        day19.part2(day19.sample) shouldBe 3621
        day19.part2(day19.actual) shouldBe 12149
      }
    }
    "day 20" - {
      "part 1" in {
        day20.part1(day20.sample) shouldBe 35
        day20.part1(day20.actual) shouldBe 5619
      }
      "part 2" in {
        day20.part2(day20.sample) shouldBe 3351
        day20.part2(day20.actual) shouldBe 20122
      }
    }
    "day 21" - {
      "part 1" in {
        day21.part1(day21.sample) shouldBe 739785
        day21.part1(day21.actual) shouldBe 995904
      }
      "part 2" in {
        day21.part2(day21.sample) shouldBe 444356092776315L
        day21.part2(day21.actual) shouldBe 193753136998081L
      }
    }
    "day 22" - {
      "part 1" in {
        day22.part1(day22.sample1) shouldBe 590784
        day22.part1(day22.actual) shouldBe 606484
      }
      "part 2" taggedAs Slow in {
        day22.part2(day22.sample2) shouldBe 2758514936282235L
        day22.part2(day22.actual) shouldBe 1162571910364852L
      }
    }
    "day 23" - {
      "part 1" in {
        day23.part1(day23.sample1) shouldBe 12521
        day23.part1(day23.actual1) shouldBe 18282
      }
      "part 2" taggedAs Slow in {
        day23.part2(day23.sample2) shouldBe 44169
        day23.part2(day23.actual2) shouldBe 50132
      }
    }
    "day 24" - {
      "part 1" in {
        day24.part1(day24.actual) shouldBe (91599994399395L, 0L)
      }
      "part 2" taggedAs Slow in {
        day24.part2(day24.actual) shouldBe (71111591176151L, 0L)
      }
    }
    "day 25" - {
      "part 1" in {
        day25.part1(day25.sample) shouldBe 58
        day25.part1(day25.actual) shouldBe 528
      }
    }
  }
