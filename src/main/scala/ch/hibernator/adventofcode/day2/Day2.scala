package ch.hibernator.adventofcode.day2

import ch.hibernator.adventofcode.SolutionBaseSimple
import ch.hibernator.adventofcode.util.MathUtil.{isOdd, nextOrderMagnitude, numDigits, previousOrderMagnitude}

import scala.util.chaining.scalaUtilChainingOps

object Day2 extends SolutionBaseSimple:
  override def day: Int = 2

  override def solve(input: Seq[String]): (Long, Long) =
    val allRanges = input.head
      .split(",")
      .map: rawRange =>
        rawRange
          .split("-")
          .pipe: startEnd =>
            SimpleRange(startEnd.head.toLong, startEnd.last.toLong)
      .toSeq

    println(allRanges)

    // make sure all ranges contain only numbers with even number of digits (useful for the first part)
    val filteredRanges = allRanges
      .filterNot(range => range.start.numDigits.isOdd && range.end.numDigits.isOdd)
      .map: range =>
        if range.start.numDigits.isOdd then SimpleRange(range.start.nextOrderMagnitude, range.end) else range
      .map: range =>
        if range.end.numDigits.isOdd then SimpleRange(range.start, range.end.previousOrderMagnitude) else range
    println(filteredRanges)

    val falseIds = filteredRanges.flatMap: range =>
      val realRange = range.start to range.end
      realRange.filter: id =>
        val (firstHalf, secondHalf) = id.toString.splitAt(id.numDigits / 2)
        firstHalf == secondHalf
    println(falseIds)
    val result1 = falseIds.sum

    // Split ranges that cross order of magnitude into two ranges
    // each of which contains only numbers with the same amount of digits
    val rangesWithEqualNumDigits = allRanges.flatMap: range =>
      if range.start.numDigits < range.end.numDigits then
        Seq(
          SimpleRange(range.start, range.start.nextOrderMagnitude - 1),
          SimpleRange(range.start.nextOrderMagnitude, range.end)
        )
      else Seq(range)
    println(rangesWithEqualNumDigits)

    val moreFalseIds = rangesWithEqualNumDigits.flatMap: range =>
      val realRange = range.start to range.end
      val numDigits = range.end.numDigits
      // max repeating sequence length is lower half of the number of digits
      // also the number of digits has to be divisible by sequence length
      val sequenceLengths = (1 to (numDigits / 2)).filter(numDigits % _ == 0)
      realRange.filter: id =>
        sequenceLengths.exists: sequenceLength =>
          id.toString.grouped(sequenceLength).toSet.size == 1
    println(moreFalseIds)
    val result2 = moreFalseIds.sum

    (result1, result2)

  case class SimpleRange(start: Long, end: Long)
