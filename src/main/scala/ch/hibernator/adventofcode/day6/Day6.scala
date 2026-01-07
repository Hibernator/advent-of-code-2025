package ch.hibernator.adventofcode.day6

import ch.hibernator.adventofcode.SolutionBaseSimple

object Day6 extends SolutionBaseSimple:
  override def day: Int = 6

  override def solve(input: Seq[String]): (Long, Long) = {
    val numbers = input.init.map: line =>
      line.split(" ").filterNot(_.isBlank).map(_.toLong).toSeq
    val operations = input.last.split(" ").filterNot(_.isBlank).toSeq

    val individualResults = operations.indices.map: index =>
      val taskNumbers = numbers.map(_(index))
      operations(index) match
        case "+" => taskNumbers.sum
        case "*" => taskNumbers.product
        case _   => sys.error("Unsupported operation")

    val grandTotal = individualResults.sum

    val operationsIndices = input.last.zipWithIndex.foldLeft(Seq[Int]()) { case (indices, (character, index)) =>
      if character == ' ' then indices else indices :+ index
    }

    val linesWithNumbers = input.init
    val lineWithOperations = input.last

    val tasksElements = operationsIndices.init
      .zip(operationsIndices.tail)
      .map: (currentIndex, nextIndex) =>
        linesWithNumbers.map: line =>
          line.substring(currentIndex, nextIndex - 1)
    :+ linesWithNumbers.map: line =>
      line.drop(operationsIndices.last)

    val individualResults2 = tasksElements.zipWithIndex.map: (taskElements, taskIndex) =>
      val indices = taskElements.head.indices
      val taskNumbers = indices.map: index =>
        taskElements.map(_(index)).filterNot(_ == ' ').mkString.toLong
      lineWithOperations(operationsIndices(taskIndex)) match
        case '+' => taskNumbers.sum
        case '*' => taskNumbers.product
        case _   => sys.error("Unsupported operation")

    val grandTotal2 = individualResults2.sum

    (grandTotal, grandTotal2)
  }
