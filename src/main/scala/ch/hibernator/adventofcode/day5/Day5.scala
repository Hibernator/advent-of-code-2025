package ch.hibernator.adventofcode.day5

import ch.hibernator.adventofcode.SolutionBaseSimple

object Day5 extends SolutionBaseSimple:
  override def day: Int = 5

  override def solve(input: Seq[String]): (Long, Long) = {
    val ranges = input
      .takeWhile(!_.isBlank)
      .map: line =>
        val rawStartEnd = line.split("-")
        SimpleRange(rawStartEnd.head.toLong, rawStartEnd.last.toLong)

    val ingredients = input.dropWhile(!_.isBlank).tail.map(_.toLong)

    val numFreshIngredients = ingredients.count: ingredient =>
      ranges.exists(_.isInRange(ingredient))

    val sortedRanges = ranges.sortBy(range => range.end - range.start).reverse

    val exclusiveRanges = sortedRanges.tail.foldLeft(Seq(sortedRanges.head)): (accRanges, range) =>
      val shortenedRange = accRanges.foldLeft(Option(range)): (currentRange, existingRange) =>
        currentRange.flatMap(_.excludeAnotherLongerRange(existingRange))
      accRanges ++ shortenedRange

    val numPotentialFreshIngredients = exclusiveRanges.foldLeft(0L): (acc, range) =>
      acc + (range.end - range.start + 1)

    (numFreshIngredients, numPotentialFreshIngredients)
  }

  case class SimpleRange(start: Long, end: Long):
    def isInRange(ingredient: Long): Boolean =
      ingredient >= start && ingredient <= end

    def isCompletelyWithin(other: SimpleRange): Boolean =
      start >= other.start && end <= other.end

    // We assume that this range doesn't fully contain the other range
    def excludeAnotherLongerRange(other: SimpleRange): Option[SimpleRange] =
      Option.unless(isCompletelyWithin(other)) {
        if end < other.start || start > other.end then this
        else if start >= other.start then copy(other.end + 1, end)
        else if end <= other.end then copy(start, other.start - 1)
        else this
      }
