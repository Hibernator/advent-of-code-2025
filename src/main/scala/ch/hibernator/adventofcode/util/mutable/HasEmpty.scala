package ch.hibernator.adventofcode.util.mutable

trait HasEmpty[T]:
  def empty: T

object HasEmpty:
  def getEmpty[T](using hasEmptyInstance: HasEmpty[T]): T = hasEmptyInstance.empty
  def apply[T](using hasEmptyInstance: HasEmpty[T]): HasEmpty[T] = hasEmptyInstance
