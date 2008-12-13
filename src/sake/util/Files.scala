package sake.util

import sake.environment._

class Files(val specs: List[String]) {
    
    def this(spec: String, specs: String*) = this(spec :: specs.toList)

    val specification = specs.filter(_.length() > 0)
    
    if (specification.size == 0)
        Exit.error("You must specify at least one non-empty string to Files(...)")
        
    def apply(): List[String] = {
        specification.foldLeft(List[String]()) { (all, spec) =>
            findFiles(spec) ::: all
        }.removeDuplicates.reverse
    }
    
    private val sep = Environment.environment.fileSeparator
    
    protected def findFiles(spec: String):List[String] = {
        validateSpec(spec)
        findFiles("", spec.split(sep).toList)
    }

    // The lists are constructed to provide a reasonably natural order.
    protected def findFiles(prefix: String, spec: List[String]): List[String] = spec match {
        case head :: tail => head match {
            case "**" => findInCurrentDirRecur(prefix).foldLeft(List[String]()) { (l, s) =>
                l ::: findFiles(s, tail)
            }
            case "*"  => findInCurrentDir(prefix).foldLeft(List[String]()) { (l, s) =>
                findFiles(s, tail) ::: l
            }
            case x    => {
                val newpath = makePath(prefix, head)
                exists(newpath) match {
                    case false => Nil
                    case true  => findFiles(newpath, tail)
                }
            }
        }
        case Nil => List(prefix)
    }

    protected def validateSpec(spec: String) = {
        """\*\*\w""".r findFirstIn spec match {
            case Some(_) => Exit.error("'**' in a specification can only be followed by '/' or end of string.")
            case None =>
        }
    }

    protected def findInCurrentDir(path: String) = {
        val file = makeFile(path)
        isDirectory(file) match {
            case false => Nil  // e.g., "/dir/file/*", which doesn't exist.
            case true  => file.contents.map(path+sep+_)
        }
    }
    
    protected def findInCurrentDirRecur(path: String): List[String] = {
        val file = makeFile(path)
        isDirectory(file) match {
            case false => List(path)  // e.g., "/dir/file/**" allowed.
            case true  => file.contents.foldLeft(List[String]()) { (list, path2) =>
                val fullpath = makePath(path, path2)
                findInCurrentDirRecur(fullpath) ::: (fullpath :: list)
            }
        }
    }

    protected def makePath(prefix: String, elem: String) = prefix match {
        case "" => elem
        case _  => prefix + sep + elem
    }
    
    protected def exists(path: String) = makeFile(path).exists
    
    protected def isDirectory(file: File) = file.exists && file.isDirectory
    
    // Hook for overriding in tests.
    protected def makeFile(path: String): File = new JavaFileWrapper(path)
}
