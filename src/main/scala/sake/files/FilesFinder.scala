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
  def apply(specification: String, moreSpecifications: String*): Vector[File] =
    apply(specification +: moreSpecifications)

  /**
   * Find files matching a sequence of glob expressions.
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
    spec.split(File.separator).toSeq match {
      case   "" +: tail => findFiles(toFile("/"),  tail)
      case head +: tail => findFiles(toFile(head), tail)
    }
  }

  // The lists are constructed to provide a reasonably natural order.
  protected def findFiles(parent: File, elems: Seq[String]): Vector[File] = elems match {
    case head +: tail =>
      if (head == "**") {
        findInDirRecursive(parent).foldLeft(Vector.empty[File]) { (seq, s) =>
          seq ++ findFiles(s, tail)
        }
      } else if (head.contains("*")) {
        findInDir(parent, head).foldLeft(Vector.empty[File]) { (seq, s) =>
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
        case Some(_) => Exit.fatal("'**' in a specification can only be followed by '/' or end of string.")
        case None =>
    }
  }

  /** Find all files in a directory. */
  def findInDir(parent: File): Seq[File] = findInDir(parent, "*")

  /** Find all files in a directory that match a pattern. */
  def findInDir(parent: File, filePattern: String): Seq[File] =
    parent.contentsFilteredBy(filePattern).toVector

  /** Find all files in one or more directories. */
  def findInDir(parents: Seq[File]): Seq[File] = findInDir(parents, "*")

  /** Find all files in one or more directories that match a pattern. */
  def findInDir(parents: Seq[File], filePattern: String): Seq[File] =
    parents.toVector.flatMap(parent => findInDir(parent, filePattern))

  /** Find all files in a directory, recursively. */
  def findInDirRecursive(parent: File): Seq[File] = findInDirRecursive(parent, "*")

  /** Find all files in a directory, recursively, that match a pattern. */
  def findInDirRecursive(parent: File, filePattern: String): Seq[File] = {
    val dirs = parent.contents filter (_.isDirectory)
    val matches = findInDir(parent, filePattern)
    matches ++ (dirs flatMap (d => findInDirRecursive(d)))
  }

  /** Find all files in one or more directories, recursively. */
  def findInDirRecursive(parents: Seq[File]): Seq[File] = findInDirRecursive(parents, "*")

  /** Find all files in one or more directories, recursively, that match a pattern. */
  def findInDirRecursive(parents: Seq[File], filePattern: String): Seq[File] =
    parents.toVector.flatMap(parent => findInDirRecursive(parent, filePattern))
}

object JavaFilesFinder extends FilesFinder {
  /** Provide a concrete implementation for the concrete class of File. */
  def toFile(parent: File, name: String): File = JavaFile(parent, name)

  /** Provide a concrete implementation for the concrete class of File. */
  def toFile(name: String): File = JavaFile(name)
}
