package ch.hibernator.adventofcode.day8

import ch.hibernator.adventofcode.SolutionBaseSimple

import scala.annotation.tailrec
import scala.collection.mutable

object Day8 extends SolutionBaseSimple:
  override def day: Int = 8

  override def solve(input: Seq[String]): (Long, Long) =
    val junctionBoxes = input.map: line =>
      val coordinates = line.split(",").map(_.toLong)
      JunctionBox(coordinates.head, coordinates(1), coordinates(2))

    val distanceToPair = mutable.Buffer[(Double, (JunctionBox, JunctionBox))]()
    for
      firstIndex <- junctionBoxes.indices.init
      secondIndex <- (firstIndex + 1) until junctionBoxes.size
    yield
      val firstBox = junctionBoxes(firstIndex)
      val secondBox = junctionBoxes(secondIndex)
      val distance = boxDistance(firstBox, secondBox)
      distanceToPair.addOne(distance, (firstBox, secondBox))
      ()

    val numConnectionsToMake = if input.size == 20 then 10 else 1000

    var circuits = Set[Circuit]()
    val distanceToPairSorted = distanceToPair.sortBy(_._1)
    val pairs = distanceToPairSorted.map(_._2)

    // Part 1: keep making connections until the needed number of connections is made
    @tailrec
    def makeConnection(connectionsMade: Int, index: Int): Unit =
      if connectionsMade == numConnectionsToMake then ()
      else
        val boxPair = pairs(index)
        circuits.find(_.contains(boxPair._1)) match
          case Some(circuit) =>
            if circuit.contains(boxPair._2)
            then makeConnection(connectionsMade + 1, index + 1)
            else
              circuits.find(_.contains(boxPair._2)) match
                case Some(secondBoxCircuit) =>
                  circuits = circuits - circuit
                  circuits = circuits - secondBoxCircuit
                  val mergedCircuit = circuit.merge(secondBoxCircuit)
                  circuits = circuits + mergedCircuit
                  makeConnection(connectionsMade + 1, index + 1)
                case None =>
                  circuits = circuits - circuit
                  circuits = circuits + circuit.add(boxPair._2)
                  makeConnection(connectionsMade + 1, index + 1)
          case None =>
            circuits.find(_.contains(boxPair._2)) match
              case Some(circuit) =>
                circuits = circuits - circuit
                circuits = circuits + circuit.add(boxPair._1)
                makeConnection(connectionsMade + 1, index + 1)
              case None =>
                circuits = circuits + Circuit(Set(boxPair._1, boxPair._2))
                makeConnection(connectionsMade + 1, index + 1)

    makeConnection(0, 0)
    val result1 = circuits.toSeq.sortBy(_.size()).reverse.take(3).map(_.size().toLong).product

    val numJunctions = input.size

    // Part 2: keep making connections until all junction boxes are part of 1 circuit
    @tailrec
    def keepConnecting(index: Int): (JunctionBox, JunctionBox) =
      if circuits.size == 1 && circuits.head.size() == numJunctions then pairs(index - 1)
      else
        val boxPair = pairs(index)
        circuits.find(_.contains(boxPair._1)) match
          case Some(circuit) =>
            if circuit.contains(boxPair._2)
            then keepConnecting(index + 1)
            else
              circuits.find(_.contains(boxPair._2)) match
                case Some(secondBoxCircuit) =>
                  circuits = circuits - circuit
                  circuits = circuits - secondBoxCircuit
                  val mergedCircuit = circuit.merge(secondBoxCircuit)
                  circuits = circuits + mergedCircuit
                  keepConnecting(index + 1)
                case None =>
                  circuits = circuits - circuit
                  circuits = circuits + circuit.add(boxPair._2)
                  keepConnecting(index + 1)
          case None =>
            circuits.find(_.contains(boxPair._2)) match
              case Some(circuit) =>
                circuits = circuits - circuit
                circuits = circuits + circuit.add(boxPair._1)
                keepConnecting(index + 1)
              case None =>
                circuits = circuits + Circuit(Set(boxPair._1, boxPair._2))
                keepConnecting(index + 1)

    val (lastBox1, lastBox2) = keepConnecting(0)
    val result2 = lastBox1.x * lastBox2.x

    (result1, result2)

  case class JunctionBox(x: Long, y: Long, z: Long)

  private def boxDistance(box1: JunctionBox, box2: JunctionBox): Double = {
    Math.sqrt(Math.pow(box1.x - box2.x, 2) + Math.pow(box1.y - box2.y, 2) + Math.pow(box1.z - box2.z, 2))
  }

  case class Circuit(boxes: Set[JunctionBox]):
    def contains(box: JunctionBox): Boolean = boxes.contains(box)
    def add(box: JunctionBox): Circuit = Circuit(boxes + box)
    def size(): Int = boxes.size
    def merge(another: Circuit): Circuit = Circuit(boxes.union(another.boxes))
