package ch.hibernator.adventofcode.day3

import ch.hibernator.adventofcode.SolutionBaseSimple

import scala.annotation.tailrec

object Day3 extends SolutionBaseSimple:
  override def day: Int = 3

  override def solve(input: Seq[String]): (Long, Long) =
    val batteryBanks = input.map(_.map(_.asDigit))
    val sumOf2Joltages = batteryBanks
      .map: bank =>
        val joltages = findTwoLargestBatteries(bank)
        (joltages._1.toString + joltages._2.toString).toLong
      .sum

    val sumOf12Joltages = batteryBanks
      .map: bank =>
        val joltages = findNLargestBatteries(bank, 12)
        joltages.map(_.toString).mkString.toLong
      .sum

    (sumOf2Joltages, sumOf12Joltages)

  private def findTwoLargestBatteries(bank: Seq[Int]): (Int, Int) =
    val firstBatteryJoltage = bank.init.max
    val firstBatteryIndex = bank.init.indexOf(firstBatteryJoltage)
    val secondBatteryJoltage = bank.drop(firstBatteryIndex + 1).max
    (firstBatteryJoltage, secondBatteryJoltage)

  private def findNLargestBatteries(bank: Seq[Int], numBatteries: Int): Seq[Int] =
    @tailrec
    def findNLargestBatteriesAcc(bank: Seq[Int], numBatteries: Int, acc: Seq[Int]): Seq[Int] =
      if numBatteries > 0 then
        val maxBatteryJoltage = bank.dropRight(numBatteries - 1).max
        val maxBatteryIndex = bank.indexOf(maxBatteryJoltage)
        findNLargestBatteriesAcc(bank.drop(maxBatteryIndex + 1), numBatteries - 1, acc :+ maxBatteryJoltage)
      else acc

    findNLargestBatteriesAcc(bank, numBatteries, Seq())
