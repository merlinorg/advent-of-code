package org.merlin.aoc
package year2023

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Test extends AnyFreeSpec with Matchers:
  "2023" - {
    "day 01" - {
      "part 1" in {
        day01.part1(day01.sample) shouldBe 142
        day01.part1(day01.actual) shouldBe 54561
      }
      "part 2" in {
        day01.part2(day01.sample2) shouldBe 281
        day01.part2(day01.actual) shouldBe 54076
      }
    }

    "day 02" - {
      "part 1" in {
        day02.part1(day02.sample) shouldBe 8
        day02.part1(day02.actual) shouldBe 2593
      }
      "part 2" in {
        day02.part2(day02.sample) shouldBe 2286
        day02.part2(day02.actual) shouldBe 54699
      }
    }

    "day 03" - {
      "part 1" in {
        day03.part1(day03.sample) shouldBe 4361
        day03.part1(day03.actual) shouldBe 528819
      }
      "part 2" in {
        day03.part2(day03.sample) shouldBe 467835
        day03.part2(day03.actual) shouldBe 80403602
      }
    }

    "day 04" - {
      "part 1" in {
        day04.part1(day04.sample) shouldBe 13
        day04.part1(day04.actual) shouldBe 32001
      }
      "part 2" in {
        day04.part2(day04.sample) shouldBe 30
        day04.part2(day04.actual) shouldBe 5037841
      }
    }

    "day 05" - {
      "part 1" in {
        day05.part1(day05.sample) shouldBe 35
        day05.part1(day05.actual) shouldBe 424490994
      }
      "part 2" in {
        day05.part2(day05.sample) shouldBe 46
        day05.part2(day05.actual) shouldBe 15290096
      }
    }

    "day 06" - {
      "part 1" in {
        day06.part1(day06.sample) shouldBe 288
        day06.part1(day06.actual) shouldBe 2065338
      }
      "part 2" in {
        day06.part2(day06.sample) shouldBe 71503
        day06.part2(day06.actual) shouldBe 34934171
      }
    }

    "day 07" - {
      "part 1" in {
        day07.part1(day07.sample) shouldBe 6440
        day07.part1(day07.actual) shouldBe 255048101
      }
      "part 2" in {
        day07.part2(day07.sample) shouldBe 5905
        day07.part2(day07.actual) shouldBe 253718286
      }
    }

    "day 08" - {
      "part 1" in {
        day08.part1(day08.sample) shouldBe 6
        day08.part1(day08.actual) shouldBe 21883
      }
      "part 2" in {
        day08.part2(day08.sample2) shouldBe 6
        day08.part2(day08.actual) shouldBe 12833235391111L
      }
    }

    "day 09" - {
      "part 1" in {
        day09.part1(day09.sample) shouldBe 114
        day09.part1(day09.actual) shouldBe 1702218515
      }
      "part 2" in {
        day09.part2(day09.sample) shouldBe 2
        day09.part2(day09.actual) shouldBe 925
      }
    }

    "day 10" - {
      "part 1" in {
        day10.part1(day10.sample) shouldBe 8
        day10.part1(day10.actual) shouldBe 6956
      }
      "part 2" in {
        day10.part2(day10.sample2) shouldBe 10
        day10.part2(day10.actual) shouldBe 455
      }
    }

    "day 11" - {
      "part 1" in {
        day11.part1(day11.sample) shouldBe 374
        day11.part1(day11.actual) shouldBe 9522407
      }
      "part 2" in {
        day11.part2(day11.sample) shouldBe 82000210
        day11.part2(day11.actual) shouldBe 544723432977L
      }
    }

    "day 12" - {
      "part 1" in {
        day12.part1(day12.sample) shouldBe 21
        day12.part1(day12.actual) shouldBe 7118
      }
      "part 2" in {
        day12.part2(day12.sample) shouldBe 525152
        day12.part2(day12.actual) shouldBe 7030194981795L
      }
    }

    "day 13" - {
      "part 1" in {
        day13.part1(day13.sample) shouldBe 405
        day13.part1(day13.actual) shouldBe 34202
      }
      "part 2" in {
        day13.part2(day13.sample) shouldBe 400
        day13.part2(day13.actual) shouldBe 34230
      }
    }

    "day 14" - {
      "part 1" in {
        day14.part1(day14.sample) shouldBe 136
        day14.part1(day14.actual) shouldBe 109833
      }
      "part 2" in {
        day14.part2(day14.sample) shouldBe 64
        day14.part2(day14.actual) shouldBe 99875
      }
    }

    "day 15" - {
      "part 1" in {
        day15.part1(day15.sample) shouldBe 1320
        day15.part1(day15.actual) shouldBe 503154
      }
      "part 2" in {
        day15.part2(day15.sample) shouldBe 145
        day15.part2(day15.actual) shouldBe 251353
      }
    }

    "day 16" - {
      "part 1" in {
        day16.part1(day16.sample) shouldBe 46
        day16.part1(day16.actual) shouldBe 7067
      }
      "part 2" in {
        day16.part2(day16.sample) shouldBe 51
        day16.part2(day16.actual) shouldBe 7324
      }
    }

    "day 17" - {
      "part 1" in {
        day17.part1(day17.sample) shouldBe 102
        day17.part1(day17.actual) shouldBe 1155
      }
      "part 2" in {
        day17.part2(day17.sample) shouldBe 94
        day17.part2(day17.actual) shouldBe 1283
      }
      "fp" - {
        "part 1" in {
          day17fp.part1(day17fp.sample) shouldBe 102
          day17fp.part1(day17fp.actual) shouldBe 1155
        }
        "part 2" in {
          day17fp.part2(day17fp.sample) shouldBe 94
          day17fp.part2(day17fp.actual) shouldBe 1283
        }
      }
      "fp alternate" - {
        "part 1" in {
          day17fpalt.part1(day17fpalt.sample) shouldBe 102
          day17fpalt.part1(day17fpalt.actual) shouldBe 1155
        }
        "part 2" in {
          day17fpalt.part2(day17fpalt.sample) shouldBe 94
          day17fpalt.part2(day17fpalt.actual) shouldBe 1283
        }
      }
    }

    "day 18" - {
      "part 1" in {
        day18.part1(day18.sample) shouldBe 62
        day18.part1(day18.actual) shouldBe 46394
      }
      "part 2" in {
        day18.part2(day18.sample) shouldBe 952408144115L
        day18.part2(day18.actual) shouldBe 201398068194715L
      }
      "alternate" - {
        "part 1" in {
          day18alt.part1(day18alt.sample) shouldBe 62
          day18alt.part1(day18alt.actual) shouldBe 46394
        }
        "part 2" in {
          day18alt.part2(day18alt.sample) shouldBe 952408144115L
          day18alt.part2(day18alt.actual) shouldBe 201398068194715L
        }
      }
    }

    "day 19" - {
      "part 1" in {
        day19.part1(day19.sample) shouldBe 19114
        day19.part1(day19.actual) shouldBe 425811
      }
      "part 2" in {
        day19.part2(day19.sample) shouldBe 167409079868000L
        day19.part2(day19.actual) shouldBe 131796824371749L
      }
    }

    "day 20" - {
      "part 1" in {
        day20.part1(day20.sample) shouldBe 11687500
        day20.part1(day20.actual) shouldBe 944750144
      }
      "part 2" in {
        day20.part2(day20.sample) shouldBe 1
        day20.part2(day20.actual) shouldBe 222718819437131L
      }
    }

    "day 21" - {
      "part 1" in {
        day21.part1(day21.sample) shouldBe 16
        day21.part1(day21.actual) shouldBe 3768
      }
      "part 2" in {
        day21.part2(day21.sample) shouldBe 167004
        day21.part2(day21.actual) shouldBe 627960775905777L
      }
      "alternate" - {
        "part 1" in {
          day21.part1(day21.sample) shouldBe 16
          day21.part1(day21.actual) shouldBe 3768
        }
        "part 2" in {
          day21.part2(day21.actual) shouldBe 627960775905777L
        }
      }
    }

    "day 22" - {
      "part 1" in {
        day22.part1(day22.sample) shouldBe 5
        day22.part1(day22.actual) shouldBe 497
      }
      "part 2" in {
        day22.part2(day22.sample) shouldBe 7
        day22.part2(day22.actual) shouldBe 67468
      }
    }

    "day 23" - {
      "part 1" in {
        day23.part1(day23.sample) shouldBe 94
        day23.part1(day23.actual) shouldBe 2070
      }
      "part 2" in {
        day23.part2(day23.sample) shouldBe 154
        day23.part2(day23.actual) shouldBe 6498
      }
    }

    "day 24" - {
      "part 1" in {
        day24.part1(day24.sample) shouldBe 2
        day24.part1(day24.actual) shouldBe 13754
      }
      "part 2" in {
        day24.part2(day24.sample) shouldBe 47
        day24.part2(day24.actual) shouldBe 711031616315001L
      }
    }

    "day 25" - {
      "part 1" in {
        day25.part1(day25.sample) shouldBe 54
        day25.part1(day25.actual) shouldBe 596376
      }
    }
  }
