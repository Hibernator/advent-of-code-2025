package ch.hibernator.adventofcode.util

enum Direction8:
  case North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest

  def opposite(direction: Direction8): Direction8 =
    direction match
      case North     => South
      case South     => North
      case West      => East
      case East      => West
      case NorthWest => SouthEast
      case SouthEast => NorthWest
      case NorthEast => SouthWest
      case SouthWest => NorthEast
