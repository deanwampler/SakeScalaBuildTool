package sake.util

import sake.environment._
import java.io.{File => JFile}

object FilesFinder {

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
    specs.foldLeft(Vector.empty[String]) { (all, spec) =>
      all ++ findFiles(spec)
    }.distinct
  }

  protected def findFiles(spec: String):List[File] = {
    validateSpec(spec)
    findFiles(spec.split(Environment.default.fileSeparator).toSeq)
  }

  // The lists are constructed to provide a reasonably natural order.
  protected def findFiles(specInPathPieces: Seq[String]): Seq[File] =
    specInPathPieces match {
      case head +: tail => head match {
        case "**" => findInCurrentDirRecur(prefix).foldLeft(Vector.empty[File] { (seq, s) =>
          seq ++ findFiles(s, tail)
        }
        case s => if (s.contains("*")) {
            findInCurrentDir(prefix, head).foldLeft(Vector.empty[File] { (seq, s) =>
            seq ++ findFiles(s, tail)
          } else {
            val newpath = File.makePath(prefix, s)
            exists(newpath) match {
              case false => Nil
              case true  => findFiles(newpath, tail)
            }
          }
        }
      }
      case one => Vector(File(one)))
      case Nil => Vector.empty[File]
  }

  protected def validateSpec(spec: String) = {
      """\*\*\w""".r findFirstIn spec match {
          case Some(_) => Exit.error("'**' in a specification can only be followed by '/' or end of string.")
          case None =>
      }
  }

  protected def findInCurrentDir(path: String, pattern: String): Seq[File] = {
    val f = new JFile(path)
    f.contentsFilteredBy(pattern) match {
      case None => Nil
      case Some(seq: Seq[_]) => seq.map(File.makePath(path, _))
    }
  }

protected def findInCurrentDirRecur(path: String): List[String] = {
  val file = new JFile(path)
  val recursiveList = isDirectory(file) match {
      case false => Seq(path)  // e.g., "/dir/file/**" allowed.
      case true  => file.contents match {
          case None => Nil
          case Some(l:List[_]) => l.foldLeft(List[String](path)) { (list, path2) =>
              val fullpath = File.makePath(path, path2)
              findInCurrentDirRecur(fullpath) ::: (fullpath :: list)
          }
      }
  }
  recursiveList match {
    case List("") => Nil
    case l => l
}
}
}
