package sake.util

import sake.environment._
import scala.collection.immutable.{Seq => ISeq}

/**
 * Encapsulate path-like structures, like CLASSPATH. The delimiter is used for toString.
 */
case class Path[T](elements: Vector[T] = Vector.empty, separator: String = ":") extends ISeq[T] {

  /**
   * Return the element at index i, zero indexed.
   */
  def apply(i: Int): T = elements(i)

  /**
   * Return an iterator through the Path.
   */
  def iterator: Iterator[T] = elements.iterator

  /**
   * Return the length.
   */
  def length: Int = elements.length

  override def toString() = elements.mkString(separator)
}

object Path {
  def apply(elements: Vector[T] = Vector.empty, separator: String = pathStringSeparator) = new Path(elements, separator)
  def apply(elements: T*, separator: String = ":") = new Path(elements.toVector, separator)
}
