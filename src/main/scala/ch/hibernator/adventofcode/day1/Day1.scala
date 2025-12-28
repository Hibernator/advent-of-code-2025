package ch.hibernator.adventofcode.day1

import ch.hibernator.adventofcode.SolutionBaseSimple
import ch.hibernator.adventofcode.day1.Day1.Rotation.Direction

object Day1 extends SolutionBaseSimple:

  override def day: Int = 1

  override def solve(input: Seq[String]): (Long, Long) =
    val rotations = input.map(Rotation.fromString)

    val (numZerosEnd, numZerosFromOverflow, numZerosFromPartialTurn, lastPosition) = rotations.foldLeft((0, 0, 0, 50)) {
      case ((numZerosEnd, numZerosFromOverflow, numZerosFromPartialTurn, currentPosition), nextRotation) =>
        val shortenedDistance = nextRotation.distance % 100
        val (newPosition, wentThroughZero) = nextRotation.direction match
          case Direction.Left =>
            val intermediatePosition = currentPosition - shortenedDistance
            val newPosition =
              if intermediatePosition >= 0 then intermediatePosition % 100
              else (100 + (intermediatePosition % 100)) % 100
            (newPosition, shortenedDistance != 0 && currentPosition != 0 && intermediatePosition <= 0)
          case Direction.Right =>
            val intermediatePosition = currentPosition + shortenedDistance
            (intermediatePosition % 100, shortenedDistance != 0 && currentPosition != 0 && intermediatePosition >= 100)
        val result = (
          if newPosition == 0 then numZerosEnd + 1 else numZerosEnd,
          numZerosFromOverflow + nextRotation.distance / 100,
          if wentThroughZero then numZerosFromPartialTurn + 1 else numZerosFromPartialTurn,
          newPosition
        )
        println(result)
        result
    }

    (numZerosEnd, numZerosFromOverflow + numZerosFromPartialTurn)

  case class Rotation(direction: Rotation.Direction, distance: Int)

  object Rotation:
    enum Direction derives CanEqual:
      case Left, Right

    def fromString(s: String): Rotation =
      val direction = s.head match
        case 'L' => Direction.Left
        case 'R' => Direction.Right
      val distance = s.tail.toInt
      Rotation(direction, distance)
