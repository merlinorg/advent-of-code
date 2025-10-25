# Advent of Code

[![Scala](https://img.shields.io/badge/Scala%203-%23de3423.svg?logo=scala&logoColor=white)](https://www.scala-lang.org/)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Scala CI](https://github.com/merlinorg/advent-of-code/actions/workflows/scala.yml/badge.svg)](https://github.com/merlinorg/advent-of-code/actions/workflows/scala.yml)

A Scala 3 winter miracle!

https://adventofcode.com/

## Run it with SBT

```shell
% sbt
sbt:aoc> runMain org.merlin.aoc.year2022.day16.part1
###
sbt:aoc> runMain org.merlin.aoc.year2022.day16.part2
###
sbt:aoc> test
...
sbt:aoc> testOnly -- -l "org.scalatest.tags.Slow"
...
sbt:aoc> testOnly -- -z "2022 day 01"
...
sbt:aoc> Jmh/run -i 3 -wi 3 -f1 -t1 -tu ms -bm avgt .*11.*
```

Input files in this repo are encrypted with [git-crypt](https://www.agwa.name/projects/git-crypt/) so, you know, use your own.
