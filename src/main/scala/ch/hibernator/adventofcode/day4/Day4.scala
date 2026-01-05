package ch.hibernator.adventofcode.day4

import cats.Show
import ch.hibernator.adventofcode.SolutionBaseSimple
import ch.hibernator.adventofcode.day4.Day4.Place.Paper
import ch.hibernator.adventofcode.util.Coordinates
import ch.hibernator.adventofcode.util.mutable.{FilledGrid, HasEmpty}

object Day4 extends SolutionBaseSimple:
  override def day: Int = 4

  override def solve(input: Seq[String]): (Long, Long) =
    val grid = FilledGrid.fromInput(input, Place.fromChar)
    println(grid.asText)

    val result1 = (for
      row <- 0 until grid.numRows
      column <- 0 until grid.numColumns
      positionWithValue = grid.getCoordinatesWithValue(row, column) if positionWithValue.value == Paper
    yield grid.conditionalNeighbors8(positionWithValue.coordinates)(_ == Paper))
      .count(_.size < 4)

    var numPapersRemoved: Int = 0
    var papersWereRemoved: Boolean = false

    def findPositionsWithRemovablePaper(): Seq[Coordinates] =
      for
        row <- 0 until grid.numRows
        column <- 0 until grid.numColumns
        positionWithPaper = grid.getCoordinatesWithValue(row, column) if positionWithPaper.value == Paper && grid
          .conditionalNeighbors8(positionWithPaper.coordinates)(_ == Paper)
          .size < 4
      yield positionWithPaper.coordinates

    def removePapers(positions: Seq[Coordinates]): Unit =
      if positions.nonEmpty then papersWereRemoved = true
      positions.foreach: position =>
        numPapersRemoved += 1
        grid.set(position, Place.Empty)

    while
      papersWereRemoved = false
      removePapers(findPositionsWithRemovablePaper())
      papersWereRemoved
    do ()

    (result1, numPapersRemoved)

  enum Place(representation: Char):
    case Empty extends Place('.')
    case Paper extends Place('@')

    override def toString: String = representation.toString

  object Place:
    def fromChar(char: Char): Place =
      char match
        case '.' => Empty
        case '@' => Paper

    given HasEmpty[Place]:
      override def empty: Place = Empty

    given Show[Place]:
      override def show(place: Place): String = place.toString
