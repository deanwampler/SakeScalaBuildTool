package sake.files

import sake.context._
import sake.util.Exit
import java.io.{File => JFile}

trait FilesFinder {

  /** Provide a concrete implementation for the concrete class of File. */
  def toFile(parent: File, name: String): File

  /** Provide a concrete implementation for the concrete class of File. */
  def toFile(name: String): File

  /**
   * Find files matching zero to many glob expressions.
   */
  def apply(specifications: String*): Vector[File] = apply(specifications.toSeq)

  /**
   * Find files matching zero to many glob expressions. Empty expressions and
   * non-matching expressions are effectively ignored. Duplicates are removed.
   */
  def apply(specifications: Seq[String]): Vector[File] = {
    val specs = specifications.filter(_.length() > 0)
    specs.foldLeft(Vector.empty[File]) { (all, spec) =>
      all ++ findFiles(spec)
    }.distinct
  }

  protected def findFiles(spec: String): Vector[File] = {
    validateSpec(spec)
    // Split on the file delimiter, then if the first element is "",
    // it means the path spec is absolute (starts at "/").
    spec.split(Properties.fileSeparator).toSeq match {
      case   "" +: tail => findFiles(toFile("/"),  tail)
      case head +: tail => findFiles(toFile(head), tail)
    }
  }

  // The lists are constructed to provide a reasonably natural order.
  protected def findFiles(parent: File, elems: Seq[String]): Vector[File] = elems match {
    case head +: tail =>
      if (head == "**") {
        findInCurrentDirRecur(parent).foldLeft(Vector.empty[File]) { (seq, s) =>
          seq ++ findFiles(s, tail)
        }
      } else if (head.contains("*")) {
        findInCurrentDir(parent, head).foldLeft(Vector.empty[File]) { (seq, s) =>
          seq ++ findFiles(s, tail)
        }
      } else {
        val newpath = toFile(parent, head)
        if (newpath.exists) findFiles(newpath, tail) else Vector.empty[File]
      }
    case Nil => Vector.empty[File]
  }

  protected def validateSpec(spec: String) = {
    """\*\*\w""".r findFirstIn spec match {
        case Some(_) => Exit.error("'**' in a specification can only be followed by '/' or end of string.")
        case None =>
    }
  }

  protected def findInCurrentDir(parent: File, pattern: String): Vector[File] =
    parent.contentsFilteredBy(pattern) match {
      case None => Vector.empty
      case Some(seq: Seq[_]) => seq.map(toFile(parent, _)).toVector
    }

  protected def findInCurrentDirRecur(parent: File): Vector[File] = {
    if (parent.isDirectory == false) Vector(parent)  // e.g., "/dir/file/**" allowed.
    else {
      parent.contents match {
        case None => Vector.empty
        case Some(seq: Seq[_]) => seq.foldLeft(Vector(parent)) { (vector, name) =>
            val file = toFile(parent, name)
            (vector :+ file) ++ findInCurrentDirRecur(file)
        }
      }
    }
  }
}

object JavaFilesFinder extends FilesFinder {
  /** Provide a concrete implementation for the concrete class of File. */
  def toFile(parent: File, name: String): File = JavaFile(parent, name)

  /** Provide a concrete implementation for the concrete class of File. */
  def toFile(name: String): File = JavaFile(name)
}
