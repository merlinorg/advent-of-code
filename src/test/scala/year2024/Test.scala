package org.merlin.aoc
package year2024

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

class Test extends AnyFreeSpec with Matchers:
  "2024" - {
    "day 01" - {
      "part 1" in {
        day01.part1(day01.sample) shouldBe 11
        day01.part1(day01.actual) shouldBe 765748
      }
      "part 2" in {
        day01.part2(day01.sample) shouldBe 31
        day01.part2(day01.actual) shouldBe 27732508
      }
    }

    "day 02" - {
      "part 1" in {
        day02.part1(day02.sample) shouldBe 2
        day02.part1(day02.actual) shouldBe 591
      }
      "part 2" in {
        day02.part2(day02.sample) shouldBe 4
        day02.part2(day02.actual) shouldBe 621
      }
    }

    "day 03" - {
      "part 1" in {
        day03.part1(day03.sample) shouldBe 161
        day03.part1(day03.actual) shouldBe 178538786
      }
      "part 2" in {
        day03.part2(day03.sample2) shouldBe 48
        day03.part2(day03.actual) shouldBe 102467299
      }
    }

    "day 04" - {
      "part 1" in {
        day04.part1(day04.sample) shouldBe 18
        day04.part1(day04.actual) shouldBe 2447
      }
      "part 2" in {
        day04.part2(day04.sample) shouldBe 9
        day04.part2(day04.actual) shouldBe 1868
      }
    }

    "day 05" - {
      "part 1" in {
        day05.part1(day05.sample) shouldBe 143
        day05.part1(day05.actual) shouldBe 4774
      }
      "part 2" in {
        day05.part2(day05.sample) shouldBe 123
        day05.part2(day05.actual) shouldBe 6004
      }
    }

    "day 06" - {
      "part 1" in {
        day06.part1(day06.sample) shouldBe 41
        day06.part1(day06.actual) shouldBe 4967
      }
      "part 2" in {
        day06.part2(day06.sample) shouldBe 6
        day06.part2(day06.actual) shouldBe 1789
      }
    }

    "day 07" - {
      "part 1" in {
        day07.part1(day07.sample) shouldBe 3749
        day07.part1(day07.actual) shouldBe 2664460013123L
      }
      "part 2" in {
        day07.part2(day07.sample) shouldBe 11387
        day07.part2(day07.actual) shouldBe 426214131924213L
      }
    }

    "day 08" - {
      "part 1" in {
        day08.part1(day08.sample) shouldBe 14
        day08.part1(day08.actual) shouldBe 371
      }
      "part 2" in {
        day08.part2(day08.sample) shouldBe 34
        day08.part2(day08.actual) shouldBe 1229
      }
    }

    "day 09" - {
      "part 1" in {
        day09.part1(day09.sample) shouldBe 1928
        day09.part1(day09.actual) shouldBe 6399153661894L
      }
      "part 2" in {
        day09.part2(day09.sample) shouldBe 2858
        day09.part2(day09.actual) shouldBe 6421724645083L
      }
    }

    "day 10" - {
      "part 1" in {
        day10.part1(day10.sample) shouldBe 36
        day10.part1(day10.actual) shouldBe 733
      }
      "part 2" in {
        day10.part2(day10.sample) shouldBe 81
        day10.part2(day10.actual) shouldBe 1514
      }
    }

    "day 11" - {
      "part 1" in {
        day11.part1(day11.sample) shouldBe 55312
        day11.part1(day11.actual) shouldBe 197157
      }
      "part 2" in {
        day11.part2(day11.sample) shouldBe 65601038650482L
        day11.part2(day11.actual) shouldBe 234430066982597L
      }
    }

    "day 12" - {
      "part 1" in {
        day12.part1(day12.sample) shouldBe 1930
        day12.part1(day12.actual) shouldBe 1381056
      }
      "part 2" in {
        day12.part2(day12.sample) shouldBe 1206
        day12.part2(day12.actual) shouldBe 834828
      }
    }

    "day 13" - {
      "part 1" in {
        day13.part1(day13.sample) shouldBe 480
        day13.part1(day13.actual) shouldBe 36954
      }
      "part 2" in {
        day13.part2(day13.sample) shouldBe 875318608908L
        day13.part2(day13.actual) shouldBe 79352015273424L
      }
    }

    "day 14" - {
      "part 1" in {
        day14.part1(day14.sample) shouldBe 12
        day14.part1(day14.actual) shouldBe 236628054
      }
      "part 2" in {
        day14.part2(day14.sample) shouldBe 0
        day14.part2(day14.actual) shouldBe 7584
      }
    }

    "day 15" - {
      "part 1" in {
        day15.part1(day15.sample) shouldBe 10092
        day15.part1(day15.actual) shouldBe 1465523
      }
      "part 2" in {
        day15.part2(day15.sample) shouldBe 9021
        day15.part2(day15.actual) shouldBe 1471049
      }
    }

    "day 16" - {
      "part 1" in {
        day16.part1(day16.sample) shouldBe 7036
        day16.part1(day16.actual) shouldBe 88468
      }
      "part 2" in {
        day16.part2(day16.sample) shouldBe 45
        day16.part2(day16.actual) shouldBe 616
      }
      "alternate" - {
        "part 1" in {
          day16alt.part1(day16alt.sample) shouldBe 7036
          day16alt.part1(day16alt.actual) shouldBe 88468
        }
        "part 2" in {
          day16alt.part2(day16alt.sample) shouldBe 45
          day16alt.part2(day16alt.actual) shouldBe 616
        }
      }
      "mutable" - {
        "part 1" in {
          day16mut.part1(day16mut.sample) shouldBe 7036
          day16mut.part1(day16mut.actual) shouldBe 88468
        }
        "part 2" in {
          day16mut.part2(day16mut.sample) shouldBe 45
          day16mut.part2(day16mut.actual) shouldBe 616
        }
      }
    }

    "day 17" - {
      "part 1" in {
        day17.part1(day17.sample) shouldBe "4,6,3,5,6,3,5,2,1,0"
        day17.part1(day17.actual) shouldBe "6,2,7,2,3,1,6,0,5"
      }
      "part 2" in {
        day17.part2(day17.sample2) shouldBe 117440
        day17.part2(day17.actual) shouldBe 236548287712877L
      }
    }

    "day 18" - {
      "part 1" in {
        day18.part1(day18.sample) shouldBe 22
        day18.part1(day18.actual) shouldBe 302
      }
      "part 2" in {
        day18.part2(day18.sample) shouldBe Loc(6, 1)
        day18.part2(day18.actual) shouldBe Loc(24, 32)
      }
    }

    "day 19" - {
      "part 1" in {
        day19.part1(day19.sample) shouldBe 6
        day19.part1(day19.actual) shouldBe 267
      }
      "part 2" in {
        day19.part2(day19.sample) shouldBe 16
        day19.part2(day19.actual) shouldBe 796449099271652L
      }
      "mutable" - {
        "part 1" in {
          day19mut.part1(day19mut.sample) shouldBe 6
          day19mut.part1(day19mut.actual) shouldBe 267
        }
        "part 2" in {
          day19mut.part2(day19mut.sample) shouldBe 16
          day19mut.part2(day19mut.actual) shouldBe 796449099271652L
        }
      }
    }

    "day 20" - {
      "part 1" in {
        day20.part1(day20.sample) shouldBe 1
        day20.part1(day20.actual) shouldBe 1263
      }
      "part 2" in {
        day20.part2(day20.sample) shouldBe 285
        day20.part2(day20.actual) shouldBe 957831
      }
    }

    "day 21" - {
      "part 1" in {
        day21.part1(day21.sample) shouldBe 126384
        day21.part1(day21.actual) shouldBe 179444
      }
      "part 2" in {
        day21.part2(day21.sample) shouldBe 154115708116294L
        day21.part2(day21.actual) shouldBe 223285811665866L
      }
    }

    "day 22" - {
      "part 1" in {
        day22.part1(day22.sample) shouldBe 37327623
        day22.part1(day22.actual) shouldBe 13185239446L
      }
      "part 2" in {
        day22.part2(day22.sample2) shouldBe 23
        day22.part2(day22.actual) shouldBe 1501
      }
      "alternate" - {
        "part 1" in {
          day22alt.part1(day22alt.sample) shouldBe 37327623
          day22alt.part1(day22alt.actual) shouldBe 13185239446L
        }
        "part 2" in {
          day22alt.part2(day22alt.sample2) shouldBe 23
          day22alt.part2(day22alt.actual) shouldBe 1501
        }
      }
    }

    "day 23" - {
      "part 1" in {
        day23.part1(day23.sample) shouldBe 7
        day23.part1(day23.actual) shouldBe 1366
      }
      "part 2" in {
        day23.part2(day23.sample) shouldBe "co,de,ka,ta"
        day23.part2(day23.actual) shouldBe "bs,cf,cn,gb,gk,jf,mp,qk,qo,st,ti,uc,xw"
      }
    }

    "day 24" - {
      "part 1" in {
        day24.part1(day24.sample) shouldBe 2024
        day24.part1(day24.actual) shouldBe 59336987801432L
      }
      "part 2" in {
        day24.part2(day24.actual) shouldBe "ctg,dmh,dvq,rpb,rpv,z11,z31,z38"
      }
    }

    "day 25" - {
      "part 1" in {
        day25.part1(day25.sample) shouldBe 3
        day25.part1(day25.actual) shouldBe 3327
      }
      "alternate" - {
        "part 1" in {
          day25alt.part1(day25alt.sample) shouldBe 3
          day25alt.part1(day25alt.actual) shouldBe 3327
        }
      }
    }
  }