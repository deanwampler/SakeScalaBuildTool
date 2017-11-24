package sake.util

/** Remove duplicates, but preserve the order. */
object Dedup {

  def apply[T](seq: Seq[T]) = {
    // Use a Set for remembering what's been seen. Return a Vector.
    val (_, vect) = seq.foldLeft( (Set.empty[T], Vector.empty[T]) ) {
      case ((set, vect), item) =>
        if (set.contains(item)) (set, vect) else (set + item, vect :+ item)
    }
    vect
  }
}
