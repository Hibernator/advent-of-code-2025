package ch.hibernator.adventofcode.util.mutable

import cats.Show
import cats.implicits.toShow
import ch.hibernator.adventofcode.util.{Coordinates, CoordinatesWithValue}

import scala.reflect.ClassTag

class FilledGrid[T](grid: Array[Array[T]])(using hasEmpty: HasEmpty[T]):
  val numRows: Int = grid.length
  val numColumns: Int = grid.head.length

  def get(row: Int, column: Int): T = grid(row)(column)
  def getCoordinatesWithValue(row: Int, column: Int): CoordinatesWithValue[T] =
    CoordinatesWithValue(Coordinates(row, column), get(row, column))
  def get(coordinates: Coordinates): T = grid(coordinates.row)(coordinates.column)

  def set(coordinates: Coordinates, value: T): Unit = grid(coordinates.row)(coordinates.column) = value

  def neighboringCoordinates4(coordinates: Coordinates): Seq[Coordinates] =
    coordinates.allNeighbors4.filter(isWithinBorders)
  def neighboringCoordinates4WithValues(coordinates: Coordinates): Seq[CoordinatesWithValue[T]] =
    neighboringCoordinates4(coordinates).map(coordinates => CoordinatesWithValue(coordinates, get(coordinates)))

  def neighboringCoordinates8(coordinates: Coordinates): Seq[Coordinates] =
    coordinates.allNeighbors8.filter(isWithinBorders)
  def neighboringCoordinates8WithValues(coordinates: Coordinates): Seq[CoordinatesWithValue[T]] =
    neighboringCoordinates8(coordinates).map(coordinates => CoordinatesWithValue(coordinates, get(coordinates)))

  def isWithinBorders(coordinates: Coordinates): Boolean = isWithinBorders(coordinates.row, coordinates.column)

  def isWithinBorders(row: Int, column: Int): Boolean = row > -1 && column > -1 && row < numRows && column < numColumns

  def conditionalNeighbors4(coordinates: Coordinates)(filter: T => Boolean): Seq[Coordinates] =
    neighboringCoordinates4WithValues(coordinates)
      .filter(coordinatesWithValue => filter(coordinatesWithValue.value))
      .map(_.coordinates)

  def conditionalNeighbors8(coordinates: Coordinates)(filter: T => Boolean): Seq[Coordinates] =
    neighboringCoordinates8WithValues(coordinates)
      .filter(coordinatesWithValue => filter(coordinatesWithValue.value))
      .map(_.coordinates)

  def asText(using Show[T]): String =
    val builder = new StringBuilder
    for
      row <- 0 until numRows
      column <- 0 until numColumns
    do
      builder.append(get(row, column).show)
      if column == numColumns - 1 then builder.append("\n")

    builder.toString()

object FilledGrid:
  def fromInput[T: {HasEmpty, ClassTag}](input: Seq[String], valueCreator: Char => T): FilledGrid[T] =
    val rawGrid = Array.fill(input.length)(Array.fill(input.head.length)(HasEmpty.apply.empty))
    for
      row <- input.indices
      column <- input.head.indices
    do rawGrid(row)(column) = valueCreator(input(row)(column))
    FilledGrid(rawGrid)
