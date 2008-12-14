package sake.util

import sake.environment._

class Files() {
    
    def apply(specifications: String*): List[String] = apply(specifications.toList)
    
    def apply(specifications: List[String]): List[String] = {
        val specs = specifications.filter(_.length() > 0)
        if (specs.size == 0)
            Exit.error("You must specify at least one non-empty string to Files(...)")
        
        specs.foldLeft(List[String]()) { (all, spec) =>
            findFiles(spec) ::: all
        }.removeDuplicates.reverse
    }
    
    private val sep = Environment.environment.fileSeparator
    
    protected def findFiles(spec: String):List[String] = {
        validateSpec(spec)
        findFiles("", spec.split(sep).toList)
    }

    // The lists are constructed to provide a reasonably natural order.
    protected def findFiles(prefix: String, specInPathPieces: List[String]): List[String] = 
        specInPathPieces match {
            case head :: tail => head match {
                case "**" => findInCurrentDirRecur(prefix).foldLeft(List[String]()) { (l, s) =>
                    l ::: findFiles(s, tail)
                }
                case s    => s.contains("*") match {
                    case true  => findInCurrentDir(prefix, head).foldLeft(List[String]()) { (l, s) =>
                        findFiles(s, tail) ::: l
                    }
                    case false => {
                        val newpath = makePath(prefix, s)
                        exists(newpath) match {
                            case false => Nil
                            case true  => findFiles(newpath, tail)
                        }
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

    protected def findInCurrentDir(path: String, pattern: String) = {
        val file = makeFile(path)
        isDirectory(file) match {
            case false => Nil  // e.g., "/dir/file/*", which doesn't exist.
            case true  => file.contentsFilteredBy(pattern).map(path+sep+_)
        }
    }
    
    protected def findInCurrentDirRecur(path: String): List[String] = {
        val file = makeFile(path)
        isDirectory(file) match {
            case false => List(path)  // e.g., "/dir/file/**" allowed.
            case true  => file.contents.foldLeft(List[String]()) { (list, path2) =>
                val fullpath = makePath(path, path2)
                findInCurrentDirRecur(fullpath) ::: (fullpath :: path :: list)
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
