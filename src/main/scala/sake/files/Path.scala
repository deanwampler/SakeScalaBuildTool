package sake.files

import sake.context.Properties

/**
 * Encapsulate path-like structures, like CLASSPATH. The delimiter is used for toString.
 */
case class Path[+T](elements: Vector[T], separator: String) extends Seq[T] {

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
  def length: Int = elements.size

  override def toString() = elements.mkString(separator)
}

object Path {
  def apply[T](elements: Vector[T] = Vector.empty, separator: String = Properties.pathSeparator) = new Path[T](elements, separator)
  def empty[T] = Path[T]()

  def make[T](elements: Seq[String], separator: String = Properties.pathSeparator)(toT: String => T) =
    new Path[T](elements.map(toT).toVector, separator)
}

