package ch.hibernator.adventofcode.day7

import cats.Show
import ch.hibernator.adventofcode.SolutionBaseSimple
import ch.hibernator.adventofcode.day7.Day7.Tile.{Beam, Empty, Start}
import ch.hibernator.adventofcode.util.Coordinates
import ch.hibernator.adventofcode.util.mutable.{FilledGrid, HasEmpty}

object Day7 extends SolutionBaseSimple:
  override def day: Int = 7

  override def solve(input: Seq[String]): (Long, Long) =
    val grid = FilledGrid.fromInput[Tile](input, Tile.fromChar)
    println(grid.asText)

    val startPosition = grid.findInRow(0, Start).get
    grid.set(startPosition, Beam)

    var splitNum = 0

    // Process rows from top to bottom, track the beams and count beam splits
    def moveBeamsInRow(row: Int): Unit =
      val currentRow = grid.getRow(row)
      val nextRow = grid.getRow(row + 1)
      currentRow.zipWithIndex.foreach: (tile, column) =>
        if tile == Beam then
          if nextRow(column) == Tile.Splitter then
            splitNum += 1
            grid.setIfWithinBorders(row + 1, column - 1, Beam)
            grid.setIfWithinBorders(row + 1, column + 1, Beam)
          else grid.set(row + 1, column, Beam)

    (0 until grid.numRows - 1).foreach(moveBeamsInRow)
    println(grid.asText)

    // Create a new grid that keeps track of number of paths leading to every position
    val gridWithPathsRaw = Array.fill(grid.numRows)(Array.fill(grid.numColumns)(TileWithNumPaths(Tile.Empty, 0L)))
    for
      row <- 0 until grid.numRows
      column <- 0 until grid.numColumns
    yield gridWithPathsRaw(row)(column) = gridWithPathsRaw(row)(column).copy(tile = grid.get(row, column))
    val gridWithPaths = FilledGrid(gridWithPathsRaw)
    gridWithPaths.set(startPosition, gridWithPaths.get(startPosition).copy(numPaths = 1L))

    // process rows from top to bottom and keep track of number of paths
    // if more than one beam merges into one, the number of paths is the sum of paths of merged beams
    (1 until grid.numRows).foreach: row =>
      (0 until grid.numColumns).foreach: column =>
        val position = gridWithPaths.get(row, column)
        if position.tile == Beam then
          val pathsFromLeft =
            if grid
                .getIfWithinBorders(row, column - 1)
                .contains(Tile.Splitter) && grid.getIfWithinBorders(row - 1, column - 1).contains(Beam)
            then gridWithPaths.get(row - 1, column - 1).numPaths
            else 0
          val pathsFromRight =
            if grid
                .getIfWithinBorders(row, column + 1)
                .contains(Tile.Splitter) && grid.getIfWithinBorders(row - 1, column + 1).contains(Beam)
            then gridWithPaths.get(row - 1, column + 1).numPaths
            else 0
          val pathsFromTop =
            if grid.getIfWithinBorders(row - 1, column).contains(Beam) then gridWithPaths.get(row - 1, column).numPaths
            else 0
          gridWithPaths.set(row, column, position.copy(numPaths = pathsFromLeft + pathsFromRight + pathsFromTop))

    val numAllPaths = gridWithPaths.getRow(grid.numRows - 1).map(_.numPaths).sum

    (splitNum, numAllPaths)

  enum Tile(val representation: Char):
    case Empty extends Tile('.')
    case Start extends Tile('S')
    case Beam extends Tile('|')
    case Splitter extends Tile('^')

    override def toString: String = representation.toString

  object Tile:
    def fromChar(char: Char): Tile =
      Tile.values.find(_.representation == char).getOrElse(sys.error(s"Unsupported character: $char"))

    given HasEmpty[Tile]:
      override def empty: Tile = Empty

    given Show[Tile]:
      override def show(place: Tile): String = place.toString

  enum LeftRight:
    case Left, Right

  case class Step(position: Coordinates, direction: LeftRight)

  case class TileWithNumPaths(tile: Tile, numPaths: Long)

  object TileWithNumPaths:
    given HasEmpty[TileWithNumPaths]:
      override def empty: TileWithNumPaths = TileWithNumPaths(Empty, 0L)
