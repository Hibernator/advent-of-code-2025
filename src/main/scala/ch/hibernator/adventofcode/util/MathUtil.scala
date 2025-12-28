package ch.hibernator.adventofcode.util

import scala.annotation.tailrec

object MathUtil:
  val zero: BigDecimal = BigDecimal("0")

  @tailrec
  def gcd(a: BigDecimal, b: BigDecimal): BigDecimal =
    if a == 0 then b else gcd(b % a, a)

  def lcm(a: BigDecimal, b: BigDecimal): BigDecimal = (a / gcd(a, b)) * b

  extension (number: Long)
    def numDigits: Int = number.toString.length
    def isOdd: Boolean = number % 2 != 0
    def nextOrderMagnitude: Long = s"1${Array.fill(numDigits)("0").mkString}".toLong
    def previousOrderMagnitude: Long = Array.fill(numDigits - 1)("9").mkString.toLong
