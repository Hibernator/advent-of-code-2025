package ch.hibernator.adventofcode.util

import ch.hibernator.adventofcode.util.Direction4.{Down, Up}
import ch.hibernator.adventofcode.util.Direction8.{East, North, NorthEast, NorthWest, South, SouthEast, SouthWest, West}

case class Coordinates(row: Int, column: Int):
  def move(direction: Direction4): Coordinates =
    direction match
      case Up               => Coordinates(row - 1, column)
      case Down             => Coordinates(row + 1, column)
      case Direction4.Left  => Coordinates(row, column - 1)
      case Direction4.Right => Coordinates(row, column + 1)

  def move(direction: Direction8): Coordinates =
    direction match
      case North     => Coordinates(row - 1, column)
      case South     => Coordinates(row + 1, column)
      case West      => Coordinates(row, column - 1)
      case East      => Coordinates(row, column + 1)
      case NorthWest => Coordinates(row - 1, column - 1)
      case NorthEast => Coordinates(row - 1, column + 1)
      case SouthEast => Coordinates(row + 1, column + 1)
      case SouthWest => Coordinates(row + 1, column - 1)

  def allNeighbors4: Seq[Coordinates] =
    Direction4.values.map(move)

  def allNeighbors8: Seq[Coordinates] =
    Direction8.values.map(move)

  override def toString: String = s"[$row,$column]"
