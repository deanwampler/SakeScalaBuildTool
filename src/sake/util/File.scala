package sake.util

import java.io.{File => JFile, FilenameFilter => JFilenameFilter}
import scala.util.matching.Regex

/** 
 * A wrapper around Java's file, to better support mocking.
 */
trait File {
    val path:String
    if (path.length == 0) throw new BuildError("File path can't be an empty string.")
    def exists: Boolean
    def isDirectory: Boolean
    def isFile: Boolean
    def contents: Option[List[String]]
    def contentsFilteredBy(nameFilter: String): Option[List[String]]
    
    def createNewFile: Boolean
    
    def mkdirs: Boolean
    def delete: Boolean
}

class FileFilter(val nameFilter: String) extends JFilenameFilter {

     val regex = ("^"+nameFilter.replaceAll("\\*", ".*").replaceAll("\\?", ".")+"$").r

     def accept(dir: JFile, name: String): Boolean = acceptName(name, regex)

     protected def acceptName(name: String, filterRegex: Regex) = {
         regex findFirstIn name match {
             case None => false
             case Some(n) => true
         }
     }
}

class JavaFileWrapper(override val path: String) extends File {

    val file = new JFile(path)
    
    def exists = file.exists()
    
    def isDirectory = file.isDirectory()
    
    def isFile = file.isFile()
    
    def contents = {
        val ary = file.list()
        if (ary == null) None else Some(ary.toList)
    }
    
    def contentsFilteredBy(nameFilter: String) = {
        val ary = file.list(new FileFilter(nameFilter))
        if (ary == null) None else Some(ary.toList)
    }
    
    def createNewFile = file.createNewFile()

    def mkdirs = file.mkdirs()
    def delete = file.delete()
}

