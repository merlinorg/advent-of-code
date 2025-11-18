package org.merlin.aoc
package year2020

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

class Test extends AnyFreeSpec with Matchers:
  "2020" - {
    "day 01" - {
      "part 1" in {
        day01.part1(day01.sample) shouldBe 514579
        day01.part1(day01.actual) shouldBe 197451
      }
      "part 2" in {
        day01.part2(day01.sample) shouldBe 241861950
        day01.part2(day01.actual) shouldBe 138233720
      }
    }
    "day 02" - {
      "part 1" in {
        day02.part1(day02.sample) shouldBe 2
        day02.part1(day02.actual) shouldBe 625
      }
      "part 2" in {
        day02.part2(day02.sample) shouldBe 1
        day02.part2(day02.actual) shouldBe 391
      }
    }
    "day 03" - {
      "part 1" in {
        day03.part1(day03.sample) shouldBe 7
        day03.part1(day03.actual) shouldBe 200
      }
      "part 2" in {
        day03.part2(day03.sample) shouldBe 336
        day03.part2(day03.actual) shouldBe 3737923200L
      }
    }
    "day 04" - {
      "part 1" in {
        day04.part1(day04.sample) shouldBe 2
        day04.part1(day04.actual) shouldBe 182
      }
      "part 2" in {
        day04.part2(day04.sample) shouldBe 2
        day04.part2(day04.actual) shouldBe 109
      }
    }
    "day 05" - {
      "part 1" in {
        day05.part1(day05.sample) shouldBe 820
        day05.part1(day05.actual) shouldBe 892
      }
      "part 2" in {
        day05.part2(day05.actual) shouldBe 625
      }
    }
    "day 06" - {
      "part 1" in {
        day06.part1(day06.sample) shouldBe 11
        day06.part1(day06.actual) shouldBe 6799
      }
      "part 2" in {
        day06.part2(day06.sample) shouldBe 6
        day06.part2(day06.actual) shouldBe 3354
      }
    }
    "day 07" - {
      "part 1" in {
        day07.part1(day07.sample) shouldBe 4
        day07.part1(day07.actual) shouldBe 332
      }
      "part 2" in {
        day07.part2(day07.sample) shouldBe 32
        day07.part2(day07.actual) shouldBe 10875
      }
    }
    "day 08" - {
      "part 1" in {
        day08.part1(day08.sample) shouldBe 5
        day08.part1(day08.actual) shouldBe 1384
      }
      "part 2" in {
        day08.part2(day08.sample) shouldBe 8
        day08.part2(day08.actual) shouldBe 761
      }
    }
    "day 09" - {
      "part 1" in {
        day09.part1(day09.sample) shouldBe 127
        day09.part1(day09.actual) shouldBe 36845998L
      }
      "part 2" in {
        day09.part2(day09.sample) shouldBe 62
        day09.part2(day09.actual) shouldBe 4830226
      }
    }
    "day 10" - {
      "part 1" in {
        day10.part1(day10.sample) shouldBe 220
        day10.part1(day10.actual) shouldBe 3034
      }
      "part 2" in {
        day10.part2(day10.sample) shouldBe 19208
        day10.part2(day10.actual) shouldBe 259172170858496L
      }
    }
    "day 11" - {
      "part 1" in {
        day11.part1(day11.sample) shouldBe 37
        day11.part1(day11.actual) shouldBe 2412
      }
      "part 2" in {
        day11.part2(day11.sample) shouldBe 26
        day11.part2(day11.actual) shouldBe 2176
      }
    }
    "day 12" - {
      "part 1" in {
        day12.part1(day12.sample) shouldBe 25
        day12.part1(day12.actual) shouldBe 1221
      }
      "part 2" in {
        day12.part2(day12.sample) shouldBe 286
        day12.part2(day12.actual) shouldBe 59435
      }
    }
    "day 13" - {
      "part 1" in {
        day13.part1(day13.sample) shouldBe 295
        day13.part1(day13.actual) shouldBe 174
      }
      "part 2" in {
        day13.part2(day13.sample) shouldBe 1068781
        day13.part2(day13.actual) shouldBe 780601154795940L
      }
    }
    "day 14" - {
      "part 1" in {
        day14.part1(day14.sample1) shouldBe 165
        day14.part1(day14.actual) shouldBe 12135523360904L
      }
      "part 2" in {
        day14.part2(day14.sample2) shouldBe 208
        day14.part2(day14.actual) shouldBe 2741969047858L
      }
    }
    "day 15" - {
      "part 1" in {
        day15.part1(day15.sample) shouldBe 436
        day15.part1(day15.actual) shouldBe 403
      }
      "part 2" in {
        day15.part2(day15.sample) shouldBe 175594
        day15.part2(day15.actual) shouldBe 6823
      }
    }
    "day 16" - {
      "part 1" in {
        day16.part1(day16.sample) shouldBe 71
        day16.part1(day16.actual) shouldBe 28884
      }
      "part 2" in {
        day16.part2(day16.sample) shouldBe 1
        day16.part2(day16.actual) shouldBe 1001849322119L
      }
    }
    "day 17" - {
      "part 1" in {
        day17.part1(day17.sample) shouldBe 112
        day17.part1(day17.actual) shouldBe 372
      }
      "part 2" in {
        day17.part2(day17.sample) shouldBe 848
        day17.part2(day17.actual) shouldBe 1896
      }
    }
    "day 18" - {
      "part 1" in {
        day18.part1(day18.sample) shouldBe 51
        day18.part1(day18.actual) shouldBe 45840336521334L
      }
      "part 2" in {
        day18.part2(day18.sample) shouldBe 51
        day18.part2(day18.actual) shouldBe 328920644404583L
      }
    }
    "day 20" - {
      "part 1" in {
        day20.part1(day20.sample) shouldBe 20899048083289L
        day20.part1(day20.actual) shouldBe 47213728755493L
      }
      "part 2" in {
        day20.part2(day20.sample) shouldBe 273
        day20.part2(day20.actual) shouldBe 1599
      }
    }
    "day 21" - {
      "part 1" in {
        day21.part1(day21.sample) shouldBe 5
        day21.part1(day21.actual) shouldBe 1829
      }
      "part 2" in {
        day21.part2(day21.sample) shouldBe "mxmxvkd,sqjhc,fvjkl"
        day21.part2(day21.actual) shouldBe "mxkh,gkcqxs,bvh,sp,rgc,krjn,bpbdlmg,tdbcfb"
      }
    }
    "day 22" - {
      "part 1" in {
        day22.part1(day22.sample) shouldBe 306
        day22.part1(day22.actual) shouldBe 32083
      }
      "part 2" in {
        day22.part2(day22.sample) shouldBe 291
        day22.part2(day22.actual) shouldBe 35495
      }
    }
    "day 23" - {
      "part 1" in {
        day23.part1(day23.sample) shouldBe 67384529
        day23.part1(day23.actual) shouldBe 25468379
      }
      "part 2" in {
        day23.part2(day23.sample) shouldBe 149245887792L
        day23.part2(day23.actual) shouldBe 474747880250L
      }
    }
    "day 24" - {
      "part 1" in {
        day24.part1(day24.sample) shouldBe 10
        day24.part1(day24.actual) shouldBe 254
      }
      "part 2" in {
        day24.part2(day24.sample) shouldBe 2208
        day24.part2(day24.actual) shouldBe 3697
      }
    }
    "day 25" - {
      "part 1" in {
        day25.part1(day25.sample) shouldBe 14897079
        day25.part1(day25.actual) shouldBe 1890859
      }
    }
  }
