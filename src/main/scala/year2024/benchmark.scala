package org.merlin.aoc
package year2024

import org.openjdk.jmh.annotations.Benchmark

class benchmark:
  @Benchmark
  def year2024_day11_1(): Unit = day11.part1(day11.actual)

  @Benchmark
  def year2024_day11_2(): Unit = day11.part2(day11.actual)

  @Benchmark
  def year2024_day16_1(): Unit = day16.part1(day16.actual)

  @Benchmark
  def year2024_day16_2(): Unit = day16.part2(day16.actual)

  @Benchmark
  def year2024_day16_alt_1(): Unit = day16alt.part1(day16alt.actual)

  @Benchmark
  def year2024_day16_alt_2(): Unit = day16alt.part2(day16alt.actual)

  @Benchmark
  def year2024_day16_mut_1(): Unit = day16mut.part1(day16mut.actual)

  @Benchmark
  def year2024_day16_mut_2(): Unit = day16mut.part2(day16mut.actual)

  @Benchmark
  def year2024_day18_2(): Unit = day18.part2(day18.actual)

  @Benchmark
  def year2024_day20_2(): Unit = day20.part2(day20.actual)

  @Benchmark
  def year2024_day22_2(): Unit = day22.part2(day22.actual)

  @Benchmark
  def year2024_day23_1(): Unit = day23.part1(day23.actual)

  @Benchmark
  def year2024_day23_2(): Unit = day23.part2(day23.actual)
