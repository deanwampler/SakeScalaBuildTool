package sake.util

import sake.environment._

class FilesFinder() {
    
    def apply(specifications: String*): List[String] = apply(specifications.toList)
    
    def apply(specifications: List[String]): List[String] = {
        val specs = specifications.filter(_.length() > 0)
        if (specs.size == 0)
            Exit.error("You must specify at least one non-empty string to FilesFinder(...)")
        
        specs.foldLeft(List[String]()) { (all, spec) =>
            findFiles(spec) ::: all
        }.distinct.reverse
    }
    
    protected def findFiles(spec: String):List[String] = {
        validateSpec(spec)
        findFiles("", spec.split(Environment.environment.fileSeparator).toList)
    }

    // The lists are constructed to provide a reasonably natural order.
    protected def findFiles(prefix: String, specInPathPieces: List[String]): List[String] = 
        specInPathPieces match {
            case head :: tail => head match {
                case "**" => findInCurrentDirRecur(prefix).foldLeft(List[String]()) { (l, s) =>
                    l ::: findFiles(s, tail)
                }
                case s => s.contains("*") match {
                    case true  => findInCurrentDir(prefix, head).foldLeft(List[String]()) { (l, s) =>
                        findFiles(s, tail) ::: l
                    }
                    case false => {
                        val newpath = File.makePath(prefix, s)
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
        makeFile(path).contentsFilteredBy(pattern) match {
            case None => Nil
            case Some(l:List[_]) => l.map(File.makePath(path, _))
        }
    }
    
    protected def findInCurrentDirRecur(path: String): List[String] = {
        val file = makeFile(path)
        val recursiveList = isDirectory(file) match {
            case false => List(path)  // e.g., "/dir/file/**" allowed.
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

    def exists(file: File):Boolean = file.exists
    
    def exists(path: String):Boolean = exists(makeFile(path))

    def isDirectory(file: File):Boolean = exists(file) && file.isDirectory

    def isDirectory(path: String):Boolean = isDirectory(makeFile(path))
    
    // Hook for overriding in tests.
    protected def makeFile(path: String): File = {
        val p = if (path.length == 0) "." else path
        new JavaFileWrapper(p)
    }
}
